#! /bin/sh

# Generate gcc's config.h, which is not your normal autoconf-generated
# config.h (that's auto-(host|build).h).  $1 is the file to generate.
# HEADERS, DEFINES, and possibly TARGET_CPU_DEFAULT are expected to be
# set in the environment.

if [ -z "$1" ]; then
    echo "Usage: HEADERS='list' DEFINES='list' mkconfig.sh FILE" >&2
    exit 1
fi

output=$1
rm -f $output.T

# We used to exec > $output.T but apparently this has bugs.
# Use a redirected subshell instead.
(

# Define TARGET_CPU_DEFAULT if the system wants one.
# This substitutes for lots of *.h files.
if [ "$TARGET_CPU_DEFAULT" != "" ]; then
    echo "#define TARGET_CPU_DEFAULT ($TARGET_CPU_DEFAULT)"
fi

# The first entry in HEADERS may be auto-host.h or auto-build.h;
# it wants to be included even when not -DIN_GCC.
if [ -n "$HEADERS" ]; then
    set $HEADERS; first=$1
    case $first in auto-* )
	echo "#include \"$first\""
	shift
	HEADERS=$*
	;;
    esac
fi

if [ -n "$HEADERS" ]; then
    echo '#ifdef IN_GCC'
    for file in $HEADERS; do
	echo "# include \"$file\""
    done
    echo '#endif'
fi

for def in $DEFINES; do
    echo "#ifndef $def" | sed 's/=.*//'
    echo "# define $def" | sed 's/=/ /'
    echo "#endif"
done

# If this is tm_p.h, include tm-preds.h unconditionally.
# If this is tconfig.h or hconfig.h, include no more files.
# Otherwise, include insn-constants.h and insn-flags.h,
# but only if GENERATOR_FILE is not defined.
case $output in
    *tm_p.h)
	echo "#include \"tm-preds.h\""
    ;;
    *tconfig.h | *hconfig.h)
    ;;
    *)
	echo "#ifndef GENERATOR_FILE"
	echo "# include \"insn-constants.h\""
	echo "# include \"insn-flags.h\""
	echo "#endif"
    ;;
esac

) > $output.T

# Avoid changing the actual file if possible.
if [ -f $output ] && cmp $output.T $output >/dev/null 2>&1; then
    echo $output is unchanged >&2
    rm -f $output.T
else
    mv -f $output.T $output
fi

# Touch a stamp file for Make's benefit.
rm -f cs-$output
echo timestamp > cs-$output
