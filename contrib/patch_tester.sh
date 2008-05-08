#!/bin/sh

# Tests a set of patches from a directory.
# Copyright (C) 2007, 2008  Free Software Foundation, Inc.
# Contributed by Sebastian Pop <sebastian.pop@amd.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

cat <<EOF

WARNING: This script should only be fed with patches from known
         authorized and trusted sources.  Don't even think about
         hooking it up to a raw feed from the gcc-patches list or
         you'll regret it.

EOF

args=$@

svnpath=svn://gcc.gnu.org/svn/gcc
dashj=
default_standby=1
standby=$default_standby
default_watermark=0.60
watermark=$default_watermark
savecompilers=false
nogpg=false
stop=false

usage() {
    cat <<EOF
patch_tester.sh [-j<N>] [-standby N] [-watermark N] [-savecompilers] [-nogpg]
                [-svnpath URL] [-stop]
                <source_dir> [patches_dir [state_dir [build_dir]]]

    J is the flag passed to make.  Default is empty string.

    STANDBY is the number of minutes between checks for new patches in
    PATCHES_DIR.  Default is ${default_standby} minutes.

    WATERMARK is the 5 minute average system charge under which a new
    compile can start.  Default is ${default_watermark}.

    SAVECOMPILERS copies the compilers in the same directory as the
    test results for the non patched version.  Default is not copy.

    NOGPG can be used to avoid checking the GPG signature of patches.

    URL is the location of the GCC SVN repository.  The default is
    ${svnpath}.

    STOP exits when PATCHES_DIR is empty.

    SOURCE_DIR is the directory containing GCC's toplevel configure.

    PATCHES_DIR is the directory containing the patches to be tested.
    Default is SOURCE_DIR/patches.

    STATE_DIR is where the tester maintains its internal state.
    Default is SOURCE_DIR/state.

    BUILD_DIR is the build tree, a temporary directory that this
    script will delete and recreate.  Default is SOURCE_DIR/obj.

EOF
    exit 1
}

makedir () {
    DIRNAME=$1
    mkdir -p $DIRNAME
    if [ $? -ne 0 ]; then
	echo "ERROR: could not make directory $DIRNAME"
	exit 1
    fi
}

while [ $# -ne 0 ]; do
    case $1 in
	-j*)
	    dashj=$1; shift
	    ;;
	-standby)
	    [[ $# > 2 ]] || usage
	    standby=$2; shift; shift
	    ;;
	-watermark)
	    [[ $# > 2 ]] || usage
	    watermark=$2; shift; shift
	    ;;
	-savecompilers)
	    savecompilers=true; shift
	    ;;
	-nogpg)
	    nogpg=true; shift
	    ;;
	-stop)
	    stop=true; shift
	    ;;
	-svnpath)
	    svnpath=$2; shift; shift
	    ;;
	-*) 
	    echo "Invalid option: $1"
	    usage
	    ;;
	*)
	    break
	    ;;
    esac
done

test $# -eq 0 && usage

SOURCE=$1
PATCHES=
STATE=
BUILD=

if [[ $# < 2 ]]; then
    PATCHES=$SOURCE/patches
else
    PATCHES=$2
fi
if [[ $# < 3 ]]; then
    STATE=$SOURCE/state
else
    STATE=$3
fi
if [[ $# < 4 ]]; then
    BUILD=$SOURCE/obj
else
    BUILD=$4
fi

[ -d $PATCHES ] || makedir $PATCHES
[ -d $STATE ] || makedir $STATE
[ -d $STATE/patched ] || makedir $STATE/patched
[ -d $SOURCE ] || makedir $SOURCE
[ -f $SOURCE/config.guess ] || {
    cd $SOURCE
    svn -q co $svnpath/trunk .
    if [ $? -ne 0 ]; then
	echo "ERROR: initial svn checkout failed"
	exit 1
    fi
}

# This can contain required local settings:
#  default_config  configure options, always passed
#  default_make    make bootstrap options, always passed
#  default_check   make check options, always passed
[ -f $STATE/defaults ] && . $STATE/defaults

VERSION=`svn info $SOURCE | grep "^Revision:" | sed -e "s/^Revision://g" -e "s/ //g"`

exec >> $STATE/tester.log 2>&1 || exit 1
set -x

TESTING=$STATE/testing
REPORT=$TESTING/report
PRISTINE=$TESTING/pristine
PATCHED=$TESTING/patched
PATCH=
TARGET=`$SOURCE/config.guess || exit 1` 
TESTLOGS="gcc/testsuite/gcc/gcc.sum
gcc/testsuite/gfortran/gfortran.sum
gcc/testsuite/g++/g++.sum
gcc/testsuite/objc/objc.sum
$TARGET/libstdc++-v3/testsuite/libstdc++.sum
$TARGET/libffi/testsuite/libffi.sum
$TARGET/libjava/testsuite/libjava.sum
$TARGET/libgomp/testsuite/libgomp.sum
$TARGET/libmudflap/testsuite/libmudflap.sum"
COMPILERS="gcc/cc1
gcc/cc1obj
gcc/cc1plus
gcc/f951
gcc/jc1
gcc/gnat1
gcc/tree1"

now () {
    echo `TZ=UTC date +"%Y_%m_%d_%H_%M_%S"`
}

report () {
    echo "$@" >> $REPORT
}

freport () {
    if [ -s $1 ]; then
	report "(cat $1"
	cat $1 >> $REPORT
	report "tac)"
    fi
}

cleanup () {
    cd $SOURCE
    svn cleanup && svn revert -R . && svn st | cut -d' ' -f5- | xargs rm -v
}

selfexec () {
    exec ${CONFIG_SHELL-/bin/sh} $0 $args
}

update () {
    svn_branch=`grep "^branch:" $PATCH | sed -e "s/^branch://g" -e "s/ //g"`
    if [ x$svn_branch = x ]; then
	svn_branch=trunk
    fi

    svn_revision=`grep "^revision:" $PATCH | sed -e "s/^revision://g" -e "s/ //g"`
    if [ x$svn_revision = x ]; then
	svn_revision=HEAD
    fi

    cleanup
    cd $SOURCE
    case $svn_branch in
	trunk)
	    if ! svn switch -r $svn_revision $svnpath/trunk &> $TESTING/svn ; then
		report "failed to update svn sources with"
		report "svn switch -r $svn_revision $svnpath/trunk"
		freport $TESTING/svn
		return 1
	    fi
	    ;;

	${svnpath}*)
	    if ! svn switch -r $svn_revision $svn_branch &> $TESTING/svn ; then
		report "failed to update svn sources with"
		report "svn switch -r $svn_revision $svn_branch"
		freport $TESTING/svn
		return 1
	    fi
	    ;;

	*)
	    if ! svn switch -r $svn_revision $svnpath/branches/$svn_branch &> $TESTING/svn ; then
		report "failed to update svn sources with"
		report "svn switch -r $svn_revision $svnpath/branches/$svn_branch"
		freport $TESTING/svn
		return 1
	    fi
	    ;;
    esac
    contrib/gcc_update --touch

    current_version=`svn info $SOURCE | grep "^Revision:" | sed -e "s/^Revision://g" -e "s/ //g"`
    if [[ $VERSION < $current_version ]]; then
	if [ -f $SOURCE/contrib/patch_tester.sh ]; then
	    selfexec
	fi
    fi

    return 0
}

apply_patch () {
    if [ $nogpg = false ]; then
	if ! gpg --batch --verify $PATCH &> $TESTING/gpgverify ; then
	    report "your patch failed to verify:"
	    freport $TESTING/gpgverify
	    return 1
	fi
    fi

    cd $SOURCE
    if ! patch -p0 < $PATCH &> $TESTING/patching ; then
	report "your patch failed to apply:"
	report "(check that the patch was created at the top level)"
	freport $TESTING/patching
	return 1
    fi

    # Just assume indexes for now -- not really great, but svn always
    # makes them.
    grep "^Index: " $PATCH | sed -e 's/Index: //' | while read file; do
	# If the patch resulted in an empty file, delete it.
	# This is how svn reports deletions.
	if [ ! -s $file ]; then
	    rm -f $file
	    report "Deleting empty file $file"
	fi
    done
}

save_compilers () {
    for COMPILER in $COMPILERS ; do
	if [ -f $BUILD/$COMPILER ]; then
	    cp $BUILD/$COMPILER $PRISTINE
	fi
    done
}

bootntest () {
    rm -rf $BUILD
    mkdir $BUILD
    cd $BUILD

    CONFIG_OPTIONS=`grep "^configure:" $PATCH | sed -e "s/^configure://g"`
    CONFIG_OPTIONS="$default_config $CONFIG_OPTIONS"
    if ! eval $SOURCE/configure $CONFIG_OPTIONS &> $1/configure ; then
	report "configure with `basename $1` version failed with:"
	freport $1/configure
	return 1
    fi

    MAKE_ARGS=`grep "^make:" $PATCH | sed -e "s/^make://g"`
    MAKE_ARGS="$default_make $MAKE_ARGS"
    if ! eval make $dashj $MAKE_ARGS &> $1/bootstrap ; then
	report "bootstrap with `basename $1` version failed with last lines:"
	tail -30 $1/bootstrap > $1/last_bootstrap
	freport $1/last_bootstrap
	report "grep --context=20 Error bootstrap:"
	grep --context=20 Error $1/bootstrap > $1/bootstrap_error
	freport $1/bootstrap_error
	return 1
    fi

    CHECK_OPTIONS=`grep "^check:" $PATCH | sed -e "s/^check://g"`
    CHECK_OPTIONS="$default_check $CHECK_OPTIONS"
    eval make $dashj $CHECK_OPTIONS -k check &> $1/check

    SUITESRUN="`grep 'Summary ===' $1/check | cut -d' ' -f 2 | sort`"
    if [ x$SUITESRUN = x ]; then
	report "check with `basename $1` version failed, no testsuites were run"
	return 1
    fi

    for LOG in $TESTLOGS ; do
	if [ -f $BUILD/$LOG ]; then
	    mv $BUILD/$LOG $1
	    mv `echo "$BUILD/$LOG" | sed -e "s/\.sum/\.log/g"` $1
	fi
    done

    return 0
}

bootntest_patched () {
    cleanup
    mkdir -p $PATCHED
    apply_patch && bootntest $PATCHED
    return $?
}

# Build the pristine tree with exactly the same options as the patch under test.
bootntest_pristine () {
    cleanup
    current_branch=`svn info $SOURCE | grep "^URL:" | sed -e "s/URL: //g" -e "s,${svnpath},,g"`
    current_version=`svn info $SOURCE | grep "^Revision:" | sed -e "s/^Revision://g" -e "s/ //g"`
    PRISTINE=$STATE/$current_branch/$current_version

    if [ -d $PRISTINE ]; then
	ln -s $PRISTINE $TESTING/pristine
	return 0
    else
	mkdir -p $PRISTINE
	ln -s $PRISTINE $TESTING/pristine
	bootntest $PRISTINE
	RETVAL=$?
	if [ $RETVAL = 0 -a $savecompilers = true ]; then
	    save_compilers
	fi
	return $RETVAL
    fi
}

regtest () {
    touch $1/report
    touch $1/passes
    touch $1/failed
    touch $1/regress

    for LOG in $TESTLOGS ; do
	NLOG=`basename $LOG`
	if [ -f $1/$NLOG ]; then
	    awk '/^FAIL: / { print "'$NLOG'",$2; }' $1/$NLOG
	fi
    done | sort | uniq > $1/failed

    comm -12 $1/failed $1/passes >> $1/regress
    NUMREGRESS=`wc -l < $1/regress | tr -d ' '`

    if [ $NUMREGRESS -eq 0 ] ; then
	for LOG in $TESTLOGS ; do
	    NLOG=`basename $LOG`
	    if [ -f $1/$NLOG ] ; then
		awk '/^PASS: / { print "'$NLOG'",$2; }' $1/$NLOG
	    fi
	done | sort | uniq | comm -23 - $1/failed > $1/passes
	echo "there are no regressions with your patch." >> $1/report
    else
	echo "with your patch there are $NUMREGRESS regressions." >> $1/report
	echo "list of regressions with your patch:" >> $1/report
	cat $1/regress >> $1/report
    fi
}

contrib_compare_tests () {
    report "comparing logs with contrib/compare_tests:"
    for LOG in $TESTLOGS ; do
 	NLOG=`basename $LOG`
 	if [ -f $PRISTINE/$NLOG -a -f $PATCHED/$NLOG ]; then
 	    $SOURCE/contrib/compare_tests $PRISTINE/$NLOG $PATCHED/$NLOG > $TESTING/compare_$NLOG
 	    freport $TESTING/compare_$NLOG
 	fi
    done
}

compare_passes () {
    regtest $PRISTINE
    cp $PRISTINE/passes $PATCHED
    regtest $PATCHED
    freport $PATCHED/report
    report "FAILs with patched version:"
    freport $PATCHED/failed
    report "FAILs with pristine version:"
    freport $PRISTINE/failed

    # contrib_compare_tests
}

write_report () {
    backup_patched=$STATE/patched/`now`
    report "The files used for the validation of your patch are stored in $backup_patched on the tester machine."

    EMAIL=`grep "^email:" $PATCH | sed -e "s/^email://g" -e "s/ //g"`
    if [ x$EMAIL != x ]; then
	mutt -s "[regtest] Results for `basename $PATCH` on $TARGET" -i $REPORT -a $PATCH $EMAIL
    fi

    mv $TESTING $backup_patched
}

announce () {
    EMAIL=`grep "^email:" $PATCH | sed -e "s/^email://g" -e "s/ //g"`
    if [ x$EMAIL != x ]; then

	START_REPORT=$TESTING/start_report
	echo "Hi, " >> $START_REPORT
	echo "I'm the automatic tester running on $TARGET." >> $START_REPORT
	echo "I just started to look at your patch `basename $PATCH`." >> $START_REPORT
	echo "Bye, your automatic tester." >> $START_REPORT
	mutt -s "[regtest] Starting bootstrap for `basename $PATCH` on $TARGET" -i $START_REPORT $EMAIL
    fi
}

# After selfexec, $TESTING is already set up.  
if [ -d $TESTING ]; then
    # The only file in $TESTING is the patch.
    PATCH=`ls -rt -1 $TESTING | head -1`
    PATCH=$TESTING/$PATCH
    if [ -f $PATCH ]; then
	bootntest_patched && bootntest_pristine && compare_passes
	write_report
    fi
fi

firstpatch=true
while true; do
    PATCH=`ls -rt -1 $PATCHES | head -1`
    if [ x$PATCH = x ]; then
	if [ $stop = true ]; then
	    if [ $firstpatch = true ]; then
		echo "No patches ready to test, quitting."
		exit 1
	    else
		echo "No more patches to test."
		exit 0
	    fi
	fi
	sleep ${standby}m
    else
	firstpatch=false
	sysload=`uptime | cut -d, -f 5`
	if [[ $sysload > $watermark ]]; then
	    # Wait a bit when system load is too high.
	    sleep ${standby}m
	else
	    mkdir -p $TESTING
	    mv $PATCHES/$PATCH $TESTING/
	    PATCH=$TESTING/$PATCH

	    announce
	    update && bootntest_patched && bootntest_pristine && compare_passes
	    write_report
	fi
    fi
done
