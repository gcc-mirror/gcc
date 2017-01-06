dnl
dnl Check if the assembler used supports disabling generation of hardware
dnl capabilities.  This is only supported by Solaris as at the moment.
dnl
dnl Defines:
dnl  HWCAP_CFLAGS='-Wa,-nH' if possible.
dnl
AC_DEFUN([GCC_CHECK_ASSEMBLER_HWCAP], [
  test -z "$HWCAP_CFLAGS" && HWCAP_CFLAGS=''

  # Restrict the test to Solaris, other assemblers (e.g. AIX as) have -nH
  # with a different meaning.
  case ${target_os} in
    solaris2*)
      ac_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -Wa,-nH"

      AC_MSG_CHECKING([for as that supports -Wa,-nH])
      AC_TRY_COMPILE([], [return 0;], [ac_hwcap_flags=yes],[ac_hwcap_flags=no])
      if test "$ac_hwcap_flags" = "yes"; then
	HWCAP_CFLAGS="-Wa,-nH $HWCAP_CFLAGS"
      fi
      AC_MSG_RESULT($ac_hwcap_flags)

      CFLAGS="$ac_save_CFLAGS"
      ;;
  esac

  AC_SUBST(HWCAP_CFLAGS)
])


dnl
dnl Check if the linker used supports linker maps to clear hardware
dnl capabilities.  This is only supported on Solaris at the moment.
dnl
dnl Defines:
dnl  HWCAP_LDFLAGS=-mclear-hwcap if possible
dnl  LD (as a side effect of testing)
dnl
AC_DEFUN([GCC_CHECK_LINKER_HWCAP], [
  test -z "$HWCAP_LDFLAGS" && HWCAP_LDFLAGS=''
  AC_REQUIRE([AC_PROG_LD])

  ac_save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LFLAGS -mclear-hwcap"

  AC_MSG_CHECKING([for -mclear-hwcap])
  AC_TRY_LINK([], [return 0;], [ac_hwcap_ldflags=yes],[ac_hwcap_ldflags=no])
  if test "$ac_hwcap_ldflags" = "yes"; then
    HWCAP_LDFLAGS="-mclear-hwcap $HWCAP_LDFLAGS"
  fi
  AC_MSG_RESULT($ac_hwcap_ldflags)

  LDFLAGS="$ac_save_LDFLAGS"

  AC_SUBST(HWCAP_LDFLAGS)

  AM_CONDITIONAL(HAVE_HWCAP, test $ac_hwcap_ldflags != no)
])
