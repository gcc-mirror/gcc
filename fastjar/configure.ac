dnl Process this file with autoconf to produce a configure script.
AC_PREREQ(2.57)
AC_INIT(jartool.h)
AM_INIT_AUTOMAKE(fastjar, 0.92-gcc)
AM_CONFIG_HEADER(config.h)

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PATH_PROG(RM, rm, /bin/rm, $PATH:/bin:/usr/bin:/usr/local/bin)
AC_PATH_PROG(CP, cp, /bin/cp, $PATH:/bin:/usr/bin:/usr/local/bin)
AC_PATH_PROG(STRIP, strip, /usr/bin/strip, $PATH:/bin:/usr/bin:/usr/local/bin)
AC_PATH_PROG(CHMOD, chmod, /bin/chmod, $PATH:/bin:/usr/bin:/usr/local/bin)
AC_EXEEXT

AM_MAINTAINER_MODE

dnl Add warning flags if we are using gcc.
if test "$GCC" = yes; then
  fastjar_warn_cflags='-W -Wall -pedantic -Wstrict-prototypes -Wmissing-prototypes -Wwrite-strings'
fi
AC_SUBST(fastjar_warn_cflags)

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_STRUCT_TM
AC_CHECK_HEADERS(fcntl.h unistd.h sys/param.h stdlib.h limits.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_OFF_T
AC_STRUCT_TM

# mkdir takes a single argument on some systems.
gcc_AC_FUNC_MKDIR_TAKES_ONE_ARG

dnl Check for type-widths
AC_COMPILE_CHECK_SIZEOF(char)
AC_COMPILE_CHECK_SIZEOF(short)
AC_COMPILE_CHECK_SIZEOF(int)
AC_COMPILE_CHECK_SIZEOF(long)
AC_CHECK_TYPES([long long],[AC_COMPILE_CHECK_SIZEOF(long long)])

dnl Check byte order
AC_C_BIGENDIAN_CROSS

AC_ARG_WITH(system-zlib,
[  --with-system-zlib      use installed libz])

ZLIBS=
ZDEPS=
ZINCS=
use_zlib=maybe
if test "$with_system_zlib" = yes; then
   AC_CHECK_LIB(z, deflate, ZLIBS=-lz, use_zlib=no)
else
   use_zlib=no
fi

if test "$use_zlib" = no; then
   # Brain dead way to find tree's zlib.
   ZDEPS='$(top_builddir)/../zlib/libz.a'
   ZLIBS="$ZDEPS -L\$(here)/../zlib/$libsubdir"
   ZINCS='-I$(top_srcdir)/../zlib'
fi
AC_SUBST(ZLIBS)
AC_SUBST(ZDEPS)
AC_SUBST(ZINCS)

# GCC LOCAL CHANGE
# We would like to our source tree to be readonly.  However when releases or
# pre-releases are generated, the man pages need to be included as they are
# converted from the texi files via perl which we don't require end users to
# have installed.
# Therefore we have --enable-generated-files-in-srcdir to do just that.

AC_MSG_CHECKING([whether to place generated files in the source directory])
  dnl generated-files-in-srcdir is disabled by default
  AC_ARG_ENABLE(generated-files-in-srcdir, 
[  --enable-generated-files-in-srcdir
                          put copies of generated files in source dir
                          intended for creating source tarballs for users
                          without texinfo, perl, bison or flex.],
      generated_files_in_srcdir=$enableval,
      generated_files_in_srcdir=no)

AC_MSG_RESULT($generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test x$generated_files_in_srcdir = xyes)

# Get the version trigger filename from the toplevel
if test "${with_gcc_version_trigger+set}" = set; then
   gcc_version_trigger=$with_gcc_version_trigger
else
   gcc_version_trigger=${srcdir}/version.c
fi
changequote(,)dnl
gcc_version_full=`grep version_string ${gcc_version_trigger} | sed -e 's/.*"\([^"]*\)".*/\1/'`
gcc_version=`echo ${gcc_version_full} | sed -e 's/\([^ ]*\) .*/\1/'`
changequote([,])dnl
AC_SUBST(gcc_version)

AC_OUTPUT(Makefile install-defs.sh)
