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
rm -f ${output}T

# Define TARGET_CPU_DEFAULT if the system wants one.
# This substitutes for lots of *.h files.
if [ "$TARGET_CPU_DEFAULT" != "" ]; then
    echo "#define TARGET_CPU_DEFAULT ($TARGET_CPU_DEFAULT)" >> ${output}T
fi

# The first entry in HEADERS may be auto-host.h or auto-build.h;
# it wants to be included even when not -DIN_GCC.
if [ -n "$HEADERS" ]; then
    set $HEADERS; first=$1
    case $first in auto-* )
	echo "#include \"$first\"" >> ${output}T
	shift
	HEADERS=$*
	;;
    esac
fi

# Provide three core typedefs used by everything, if we are compiling
# GCC.  These used to be found in rtl.h and tree.h, but this is no
# longer practical. Providing these in config.h/tconfig.h/hconfig.h
# rather than system.h allows the typedefs to be used anywhere in GCC.
case $output in 
    *config.h | *hconfig.h | *tconfig.h)
        cat >> ${output}T <<EOF
#ifdef IN_GCC
/* Provide three core typedefs used by everything, if we are compiling
   GCC.  These used to be found in rtl.h and tree.h, but this is no
   longer practical.  Providing these here rather that system.h allows
   the typedefs to be used everywhere within GCC. */
struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
#endif
EOF
        ;;
esac

if [ -n "$HEADERS" ]; then
    echo '#ifdef IN_GCC' >> ${output}T
    for file in $HEADERS; do
	echo "# include \"$file\"" >> ${output}T
    done
    echo '#endif' >> ${output}T
fi

for def in $DEFINES; do
    echo "#ifndef $def" | sed 's/=.*//' >> ${output}T
    echo "# define $def" | sed 's/=/ /' >> ${output}T
    echo "#endif" >> ${output}T
done

# If this is tm_p.h, include tm-preds.h unconditionally.
# If this is tconfig.h or hconfig.h, include no more files.
# Otherwise, include insn-constants.h and insn-flags.h,
# but only if GENERATOR_FILE is not defined.
case $output in
    *tm_p.h)
	echo "#include \"tm-preds.h\"" >> ${output}T
    ;;
    *tconfig.h | *hconfig.h)
    ;;
    *)
        cat >> ${output}T <<EOF
#ifndef GENERATOR_FILE
# include "insn-constants.h"
# include "insn-flags.h"
#endif
EOF
    ;;
esac

# Avoid changing the actual file if possible.
if [ -f $output ] && cmp ${output}T $output >/dev/null 2>&1; then
    echo $output is unchanged >&2
    rm -f ${output}T
else
    mv -f ${output}T $output
fi

# Touch a stamp file for Make's benefit.
rm -f cs-$output
echo timestamp > cs-$output
