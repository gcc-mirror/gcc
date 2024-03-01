#! /bin/sh

if [ $# -ne 1 ]
then
  echo "Usage: $0 <target-mach-triplet>"
  exit 1
fi

machine=$1
target=fixinc.sh

# Check for special fix rules for particular targets
case $machine in
    i?86-*-cygwin* | \
    *-mingw32* | \
    powerpc-*-eabisim* | \
    powerpc-*-eabi*    | \
    powerpc-*-rtems*   | \
    powerpcle-*-eabisim* | \
    powerpcle-*-eabi* | \
    *-*-vxworks7* | \
    *-musl* )
	#  IF there is no include fixing,
	#  THEN create a no-op fixer and exit
	(echo "#! /bin/sh" ; echo "exit 0" ) > ${target}
        ;;

    *)
	cat < ${srcdir}/fixinc.in > ${target} || exit 1
	;;
esac
chmod 755 ${target}
