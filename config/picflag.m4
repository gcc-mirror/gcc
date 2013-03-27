# _GCC_PICFLAG(FLAG, DISPATCH)
# ----------------------------
# Store PIC flag corresponding to DISPATCH triplet in FLAG.
# Explit use of -fpic in CFLAGS corresponding to FLAG overrides default.
AC_DEFUN([_GCC_PICFLAG], [

case "${$2}" in
    # PIC is the default on some targets or must not be used.
    *-*-darwin*)
	# PIC is the default on this platform
	# Common symbols not allowed in MH_DYLIB files
	$1=-fno-common
	;;
    alpha*-dec-osf5*)
	# PIC is the default.
	;;
    hppa*64*-*-hpux*)
	# PIC is the default for 64-bit PA HP-UX.
	;;
    i[[34567]]86-*-cygwin* | x86_64-*-cygwin*)
	;;
    i[[34567]]86-*-mingw* | x86_64-*-mingw*)
	;;
    i[[34567]]86-*-interix[[3-9]]*)
	# Interix 3.x gcc -fpic/-fPIC options generate broken code.
	# Instead, we relocate shared libraries at runtime.
	;;
    i[[34567]]86-*-nto-qnx*)
	# QNX uses GNU C++, but need to define -shared option too, otherwise
	# it will coredump.
	$1='-fPIC -shared'
	;;
    i[[34567]]86-pc-msdosdjgpp*)
	# DJGPP does not support shared libraries at all.
	;;
    ia64*-*-hpux*)
	# On IA64 HP-UX, PIC is the default but the pic flag
	# sets the default TLS model and affects inlining.
	$1=-fPIC
	;;
    mips-sgi-irix6*)
	# PIC is the default.
	;;
    rs6000-ibm-aix* | powerpc-ibm-aix*)
	# All AIX code is PIC.
	;;

    # Some targets support both -fPIC and -fpic, but prefer the latter.
    # FIXME: Why?
    i[[34567]]86-*-* | x86_64-*-*)
	$1=-fpic
	;;
    m68k-*-*)
	$1=-fpic
	;;
    # FIXME: Override -fPIC default in libgcc only? 
    sh-*-linux* | sh[[2346lbe]]*-*-linux*)
	$1=-fpic
	;;
    # FIXME: Simplify to sh*-*-netbsd*?
    sh-*-netbsdelf* | shl*-*-netbsdelf* | sh5-*-netbsd* | sh5l*-*-netbsd* | \
      sh64-*-netbsd* | sh64l*-*-netbsd*)
	$1=-fpic
	;;
    # Default to -fPIC unless specified otherwise.
    *)
	$1=-fPIC
	;;
esac

# If the user explicitly uses -fpic/-fPIC, keep that.
case "${m4_bpatsubsts($1, PICFLAG, CFLAGS)}" in
    *-fpic*)
	$1=-fpic
	;;
    *-fPIC*)
	$1=-fPIC
	;;
esac
])

# GCC_PICFLAG
# -----------
# Store host PIC flag in PICFLAG.
AC_DEFUN([GCC_PICFLAG], [
  AC_REQUIRE([AC_CANONICAL_HOST])
  _GCC_PICFLAG([PICFLAG], [host])])

# GCC_PICFLAG_FOR_TARGET
# ----------------------
# Store target PIC flag in PICFLAG_FOR_TARGET.
AC_DEFUN([GCC_PICFLAG_FOR_TARGET], [
  AC_REQUIRE([AC_CANONICAL_TARGET])
  _GCC_PICFLAG([PICFLAG_FOR_TARGET], [target])])
