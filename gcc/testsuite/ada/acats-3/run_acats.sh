#!/bin/sh

if [ "$testdir" = "" ]; then
   echo You must use make check or make check-ada
   exit 1
fi

# Provide which replacement.
#
# type -p is missing from Solaris 2 /bin/sh and /bin/ksh (ksh88), but both
# ksh93 and bash have it.
# type output format differs between ksh88 and ksh93, so avoid it if
# type -p is present.  Unfortunately, HP-UX /bin/sh ignores -p with type.
# Fall back to whence which ksh88 and ksh93 provide, but bash does not.

which () {
    path=`type -p $* 2>/dev/null` && { echo $path | awk '{print $NF}'; return 0; }
    path=`type $* 2>/dev/null` && { echo $path | awk '{print $NF}'; return 0; }
    path=`whence $* 2>/dev/null` && { echo $path; return 0; }
    return 1
}

# Set up environment to use the Ada compiler from the object tree

host_gnatchop=`which gnatchop`
host_gnatmake=`which gnatmake`
ROOT=`${PWDCMD-pwd}`
BASE=`cd $ROOT/../../..; ${PWDCMD-pwd}`

PATH=$BASE:$ROOT:$PATH
ADA_INCLUDE_PATH=$BASE/ada/rts
LD_LIBRARY_PATH=$ADA_INCLUDE_PATH:$BASE:$LD_LIBRARY_PATH
ADA_OBJECTS_PATH=$ADA_INCLUDE_PATH

if [ ! -d $ADA_INCLUDE_PATH ]; then
   echo gnatlib missing, exiting.
   exit 1
fi

if [ ! -f $BASE/gnatchop ]; then
   echo gnattools missing, exiting.
   exit 1
fi

if [ ! -f $BASE/gnatmake ]; then
   echo gnattools missing, exiting.
   exit 1
fi

export PATH ADA_INCLUDE_PATH ADA_OBJECTS_PATH BASE LD_LIBRARY_PATH

echo '#!/bin/sh' > host_gnatchop
echo PATH=`dirname $host_gnatchop`:'$PATH' >> host_gnatchop
echo unset ADA_INCLUDE_PATH ADA_OBJECTS_PATH GCC_EXEC_PREFIX >> host_gnatchop
echo export PATH >> host_gnatchop
echo exec gnatchop '"$@"' >> host_gnatchop

chmod +x host_gnatchop

echo '#!/bin/sh' > host_gnatmake
echo PATH=`dirname $host_gnatmake`:'$PATH' >> host_gnatmake
echo unset ADA_INCLUDE_PATH ADA_OBJECTS_PATH GCC_EXEC_PREFIX >> host_gnatmake
echo export PATH >> host_gnatmake
echo exec gnatmake '"$@"' >> host_gnatmake

chmod +x host_gnatmake

# Limit the stack to 16MB for stack checking
ulimit -s 16384

exec $testdir/run_all.sh ${1+"$@"}
