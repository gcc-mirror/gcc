# This macro wraps AC_SYS_LARGEFILE with one exception for Solaris.
# PR binutils/9992: We have to replicate everywhere the behaviour of
# bfd's configure script so that all the directories agree on the size
# of structures used to describe files.

AC_DEFUN([ACX_LARGEFILE],[dnl

# The tests for host and target for $enable_largefile require
# canonical names.
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([AC_CANONICAL_TARGET])

# As the $enable_largefile decision depends on --enable-plugins we must set it
# even in directories otherwise not depending on the $plugins option.

AC_PLUGINS

case "${host}" in
  sparc-*-solaris*|i?86-*-solaris*)
    # On native 32-bit Solaris/SPARC and x86, large-file and procfs support
    # were mutually exclusive until Solaris 11.3.  Without procfs support,
    # the bfd/ elf module cannot provide certain routines such as
    # elfcore_write_prpsinfo or elfcore_write_prstatus.  So unless the user
    # explicitly requested large-file support through the
    # --enable-largefile switch, disable large-file support in favor of
    # procfs support.
    #
    # Check if <sys/procfs.h> is incompatible with large-file support.
    AC_TRY_COMPILE([#define _FILE_OFFSET_BITS 64
#define _STRUCTURED_PROC 1
#include <sys/procfs.h>], , acx_cv_procfs_lfs=yes, acx_cv_procfs_lfs=no)
    #
    # Forcefully disable large-file support only if necessary, gdb is in
    # tree and enabled.
    if test "${target}" = "${host}" -a "$acx_cv_procfs_lfs" = no \
         -a -d $srcdir/../gdb -a "$enable_gdb" != no; then
      : ${enable_largefile="no"}
      if test "$plugins" = yes; then
	AC_MSG_WARN([
plugin support disabled; require large-file support which is incompatible with GDB.])
	plugins=no
      fi
    fi
    #
    # Explicitly undef _FILE_OFFSET_BITS if enable_largefile=no for the
    # benefit of g++ 9+ which predefines it on Solaris.
    if test "$enable_largefile" = no; then
      LARGEFILE_CPPFLAGS="-U_FILE_OFFSET_BITS"
      AC_SUBST(LARGEFILE_CPPFLAGS)
    fi
    ;;
esac

AC_SYS_LARGEFILE
])
