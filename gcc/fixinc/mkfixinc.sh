#! /bin/sh

machine=$1
if [ -z "$machine" ]
then
	echo No machine name given
	exit 1
fi

target=../fixinc.sh

echo constructing ${target} for $machine
fixincludes="${machine}"

case $machine in
	i[34567]86-*-linux-gnu*)
		:
		;;

	*-*-sysv4*)
		fixincludes=fixinc.svr4
		;;

	i[34567]86-*-sysv5* | \
	i[34567]86-*-udk* | \
	i[34567]86-*-solaris2.[0-4] | \
	powerpcle-*-solaris2.[0-4] | \
	sparc-*-solaris2.[0-4] )
		fixincludes=fixinc.svr4
		;;

	i[34567]86-*-interix* | \
	alpha-*-interix*)
		fixincludes=fixinc.interix
		;;

	*-*-netbsd* | \
	alpha*-*-linux-gnulibc1* | \
	i[34567]86-*-openbsd* | \
	i[34567]86-*-solaris2* | \
	sparcv9-*-solaris2* | \
	powerpcle-*-solaris2*  | \
	sparc-*-solaris2* )
		fixincludes=fixinc.wrap
		;;

	alpha*-*-winnt* | \
	i[34567]86-*-winnt3*)
		fixincludes=fixinc.winnt
		;;

	i[34567]86-sequent-ptx* | i[34567]86-sequent-sysv[34]*)
		fixincludes=fixinc.ptx
		;;

	i[34567]86-*-sco3.2v5* | \
	i[34567]86-*-sco3.2v4*)
		fixincludes=fixinc.sco
		;;

	alpha*-dec-vms* | \
	arm-semi-aout | armel-semi-aout | \
	arm-semi-aof | armel-semi-aof | \
	arm-*-gnu* | \
	c*-convex-* | \
	hppa1.1-*-osf* | \
	hppa1.0-*-osf* | \
	hppa1.1-*-bsd* | \
	hppa1.0-*-bsd* | \
	hppa*-*-lites* | \
	*-*-linux-gnu* | \
	*-*-gnu* | \
	i[34567]86-moss-msdos* | i[34567]86-*-moss* | \
	i[34567]86-*-osf1* | \
	i[34567]86-*-win32 | \
	i[34567]86-*-pe | i[34567]86-*-cygwin* | \
	i[34567]86-*-mingw32* | \
	i[34567]86-*-uwin* | \
	mips-sgi-irix5cross64 | \
	powerpc-*-eabiaix* | \
	powerpc-*-eabisim* | \
	powerpc-*-eabi*    | \
	powerpc-*-rtems*   | \
	powerpcle-*-eabisim* | \
	powerpcle-*-eabi*  | \
        powerpcle-*-winnt* | \
	powerpcle-*-pe | powerpcle-*-cygwin* | \
	thumb-*-coff* | thumbel-*-coff* )
		fixincludes=
		;;

	*-sgi-irix*)
		fixincludes=fixinc.irix
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
#
echo $MAKE SHELL=\"$SHELL\" install-bin

#  make and install either the binary or the default script
#
$MAKE SHELL="$SHELL" install-bin
