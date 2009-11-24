dnl A function to check for zlib availability.  zlib is used by default
dnl unless the user configured with --disable-nls.

AC_DEFUN([AM_ZLIB],
[
  # See if the user specified whether he wants zlib support or not.
  AC_ARG_WITH(zlib,
    [  --with-zlib             include zlib support (auto/yes/no) [default=auto]],
    [], [with_zlib=auto])

  if test "$with_zlib" != "no"; then
    AC_SEARCH_LIBS(zlibVersion, z, [AC_CHECK_HEADERS(zlib.h)])
    if test "$with_zlib" = "yes" -a "$ac_cv_header_zlib_h" != "yes"; then
      AC_MSG_ERROR([zlib (libz) library was explicitly requested but not found])
    fi
  fi
])

