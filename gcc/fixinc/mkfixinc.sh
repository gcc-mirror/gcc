#! /bin/sh

if [ $# -ne 2 ]
then
  echo "Usage: $0 <build-mach-triplet> <target-mach-triplet>"
  exit 1
fi

build=$1
machine=$2
target=../fixinc.sh

echo constructing ${target} for $machine to run on $build
fixincludes="${machine}"

# Choose one or two-process fix methodology.  Systems that cannot handle
# bi-directional pipes must use the two process method.
#
case $build in
	i?86-*-msdosdjgpp* | \
	*-*-beos* )
		MAKE="${MAKE} TARGETS=twoprocess"
		CFLAGS="${CFLAGS} -DSEPARATE_FIX_PROC"
		;;

	vax-dec-bsd* )
		CFLAGS="${CFLAGS} -Dexit=xexit -Datexit=xatexit"
		MAKE="${MAKE} TARGETS=oneprocess"
		;;

	* )
		MAKE="${MAKE} TARGETS=oneprocess"
		;;
esac

# Check for special fix rules for particular targets
case $machine in
    i?86-*-sysv4.2uw2* )
        ;;

    *-*-sysv4* )
        fixincludes=fixinc.svr4
        ;;

    i?86-*-interix* | \
    alpha-*-interix*)
        fixincludes=fixinc.interix
        ;;

    i?86-*-openbsd*)
        fixincludes=fixinc.wrap
        ;;

    alpha*-*-winnt* | \
    i?86-*-winnt3*)
        fixincludes=fixinc.winnt
        ;;

    i?86-sequent-ptx* | i?86-sequent-sysv[34]*)
        fixincludes=fixinc.ptx
        ;;

    alpha*-dec-*vms* | \
    arm-semi-aout | \
    armel-semi-aout | \
    arm-semi-aof | \
    armel-semi-aof | \
    c*-convex-* | \
    hppa1.1-*-osf* | \
    hppa1.0-*-osf* | \
    hppa1.1-*-bsd* | \
    hppa1.0-*-bsd* | \
    hppa*-*-lites* | \
    i?86-moss-msdos* | \
    i?86-*-moss* | \
    i?86-*-osf1* | \
    i?86-*-win32 | \
    i?86-*-pe | \
    i?86-*-cygwin* | \
    i?86-*-mingw32* | \
    i?86-*-uwin* | \
    mips-sgi-irix5cross64 | \
    powerpc-*-eabiaix* | \
    powerpc-*-eabisim* | \
    powerpc-*-eabi*    | \
    powerpc-*-rtems*   | \
    powerpcle-*-eabisim* | \
    powerpcle-*-eabi*  | \
    powerpcle-*-winnt* | \
    powerpcle-*-pe | \
    powerpcle-*-cygwin* | \
    thumb-*-coff* | \
    thumbel-*-coff* )
        #  Don't do any fixing.
        #
        fixincludes=
        ;;
esac

#  IF there is no include fixing,
#  THEN create a no-op fixer and exit
#
if test -z "$fixincludes"
then
    (echo "#! /bin/sh" ; echo "exit 0" ) > ${target}
    chmod 755 ${target}
    exit 0
fi

#  IF the fixer is supplied in our source directory,
#  THEN copy that into place
#
if test -f ${srcdir}/"${fixincludes}"
then
    echo copying ${srcdir}/$fixincludes to ${target}
    cp ${srcdir}/$fixincludes ${target}
    chmod 755 ${target}
    exit 0
fi

#  OK.  We gotta make the thing.
#  make and install either the binary or the default script

defs="SHELL=\"$SHELL\" CC=\"$CC\" CFLAGS=\"$CFLAGS\" LDFLAGS=\"$LDFLAGS\""
cmd="$MAKE ${defs} install-bin"
echo $cmd
eval $cmd
