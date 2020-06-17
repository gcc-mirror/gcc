dnl
dnl GCC_CET_FLAGS
dnl    (SHELL-CODE_HANDLER)
dnl
AC_DEFUN([GCC_CET_FLAGS],[dnl
GCC_ENABLE(cet, auto, ,[enable Intel CET in target libraries],
	   permit yes|no|auto)
AC_MSG_CHECKING([for CET support])

# NB: Avoid nested save_CFLAGS and save_LDFLAGS.
case "$host" in
  i[[34567]]86-*-linux* | x86_64-*-linux*)
    case "$enable_cet" in
      auto)
	# Check if target supports multi-byte NOPs
	# and if assembler supports CET insn.
	cet_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS -fcf-protection"
	AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM(
	  [],
	  [
#if !defined(__SSE2__)
#error target does not support multi-byte NOPs
#else
asm ("setssbsy");
#endif
	  ])],
	 [enable_cet=yes],
	 [enable_cet=no])
	CFLAGS="$cet_save_CFLAGS"
	;;
      yes)
	# Check if assembler supports CET.
	AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM(
	  [],
	  [asm ("setssbsy");])],
	 [],
	 [AC_MSG_ERROR([assembler with CET support is required for --enable-cet])])
	;;
    esac
    ;;
  *)
    enable_cet=no
    ;;
esac
if test x$enable_cet = xyes; then
  $1="-fcf-protection -mshstk"
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
])

dnl
dnl GCC_CET_HOST_FLAGS
dnl    (SHELL-CODE_HANDLER)
dnl
AC_DEFUN([GCC_CET_HOST_FLAGS],[dnl
GCC_ENABLE(cet, auto, ,[enable Intel CET in host libraries],
	   permit yes|no|auto)
AC_MSG_CHECKING([for CET support])

case "$host" in
  i[[34567]]86-*-linux* | x86_64-*-linux*)
    may_have_cet=yes
    cet_save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -fcf-protection"
    case "$enable_cet" in
      auto)
	# Check if target supports multi-byte NOPs
	# and if assembler supports CET insn.
	AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM(
	  [],
	  [
#if !defined(__SSE2__)
#error target does not support multi-byte NOPs
#else
asm ("setssbsy");
#endif
	  ])],
	 [enable_cet=yes],
	 [enable_cet=no])
	;;
      yes)
	# Check if assembler supports CET.
	AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM(
	  [],
	  [asm ("setssbsy");])],
	 [],
	 [AC_MSG_ERROR([assembler with CET support is required for --enable-cet])])
	;;
    esac
    CFLAGS="$cet_save_CFLAGS"
    ;;
  *)
    may_have_cet=no
    enable_cet=no
    ;;
esac

cet_save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -fcf-protection=none"
cet_save_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS -Wl,-z,ibt,-z,shstk"
if test x$may_have_cet = xyes; then
  # Check whether -fcf-protection=none -Wl,-z,ibt,-z,shstk work.
  AC_TRY_LINK(
    [],[return 0;],
    [may_have_cet=yes],
    [may_have_cet=no])
fi

if test x$may_have_cet = xyes; then
  if test x$cross_compiling = xno; then
    AC_TRY_RUN([
static void
foo (void)
{
}

static void
__attribute__ ((noinline, noclone))
xxx (void (*f) (void))
{
  f ();
}

static void
__attribute__ ((noinline, noclone))
bar (void)
{
  xxx (foo);
}

int
main ()
{
  bar ();
  return 0;
}
    ],
    [have_cet=no],
    [have_cet=yes])
    if test x$enable_cet = xno -a x$have_cet = xyes; then
      AC_MSG_ERROR([Intel CET must be enabled on Intel CET enabled host])
    fi
  fi
else
  # Enable CET in cross compiler if possible so that it will run on both
  # CET and non-CET hosts.
  have_cet=yes
fi
if test x$enable_cet = xyes; then
  $1="-fcf-protection"
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
CFLAGS="$cet_save_CFLAGS"
LDFLAGS="$cet_save_LDFLAGS"
])
