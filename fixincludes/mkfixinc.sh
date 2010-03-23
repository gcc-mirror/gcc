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
    alpha*-dec-*vms* | \
    i?86-moss-msdos* | \
    i?86-*-pe | \
    i?86-*-cygwin* | \
    i?86-*-interix* | \
    *-*-vxworks* | \
    powerpc-*-eabisim* | \
    powerpc-*-eabi*    | \
    powerpc-*-rtems*   | \
    powerpcle-*-eabisim* | \
    powerpcle-*-eabi* )
	#  IF there is no include fixing,
	#  THEN create a no-op fixer and exit
	(echo "#! /bin/sh" ; echo "exit 0" ) > ${target}
        ;;

    i?86-*-mingw32* | \
    x86_64-*-mingw32*)
        # We only want to fix stdlib.h in mingw.
	# FIXME: Is SED available on mingw? 
	(cat > ${target} << EOF
#! /bin/sh

OUTPUT_DIR=\$1
if [ "x\$OUTPUT_DIR" = "x" ]; then
  echo fixincludes: no output directory specified
  exit 1
fi

if [ ! -d \$OUTPUT_DIR ]; then
  echo fixincludes: output dir \"\$OUTPUT_DIR\" is an invalid directory
  exit 1
fi

INPUT_DIR=\$2
if [ "x\$INPUT_DIR" = "x" ]; then
  echo fixincludes: no input directory specified
  exit 1
fi

if [ ! -d \$INPUT_DIR ]; then
  echo fixincludes: input dir \"\$INPUT_DIR\" is an invalid directory
  exit 1
fi

INPUT_STDLIB_H=\$INPUT_DIR/stdlib.h
if [ ! -f \$INPUT_STDLIB_H ]; then
  echo fixincludes: \"stdlib.h\" is an invalid file
  exit 1
fi

OUTPUT_STDLIB_H=\$OUTPUT_DIR/stdlib.h
sed -e "s/\(.*_rotl.*\)/#if __GNUC__ < 4 || (__GNUC__ == 4 \&\& __GNUC_MINOR__ < 5)\n\1/" \
    -e "s/\(.*_lrotr.*\)/\1\n#else\n\#include <x86intrin.h>\n#endif/" \
   \$INPUT_STDLIB_H > \$OUTPUT_STDLIB_H

exit 0
EOF
)
        ;;

    *)
	cat < ${srcdir}/fixinc.in > ${target} || exit 1
	;;
esac
chmod 755 ${target}
