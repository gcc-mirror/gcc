# AC_PROG_INSTALL
# ---------------
#
# This macro is stolen from Autoconf 2.61a-341.
# It requires `install' to be able to install multiple files at once.
# This file will be obsolete when GCC moves to Autoconf 2.62.

m4_version_prereq([2.62], [],
[
AC_DEFUN([AC_PROG_INSTALL],
[AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT])dnl
dnl AC_REQUIRE_AUX_FILE([install-sh])dnl  This line does not work with 2.59.
# Find a good install program.  We prefer a C program (faster),
# so one script is as good as another.  But avoid the broken or
# incompatible versions:
# SysV /etc/install, /usr/sbin/install
# SunOS /usr/etc/install
# IRIX /sbin/install
# AIX /bin/install
# AmigaOS /C/install, which installs bootblocks on floppy discs
# AIX 4 /usr/bin/installbsd, which doesn't work without a -g flag
# AFS /usr/afsws/bin/install, which mishandles nonexistent args
# SVR4 /usr/ucb/install, which tries to use the nonexistent group "staff"
# OS/2's system install, which has a completely different semantic
# ./install, which can be erroneously created by make from ./install.sh.
# Reject install programs that cannot install multiple files.
AC_MSG_CHECKING([for a BSD-compatible install])
if test -z "$INSTALL"; then
AC_CACHE_VAL(ac_cv_path_install,
[_AS_PATH_WALK([$PATH],
[# Account for people who put trailing slashes in PATH elements.
case $as_dir/ in
  ./ | .// | /[cC]/* | \
  /etc/* | /usr/sbin/* | /usr/etc/* | /sbin/* | /usr/afsws/bin/* | \
  ?:[\\/]os2[\\/]install[\\/]* | ?:[\\/]OS2[\\/]INSTALL[\\/]* | \
  /usr/ucb/* ) ;;
  *)
    # OSF1 and SCO ODT 3.0 have their own names for install.
    # Don't use installbsd from OSF since it installs stuff as root
    # by default.
    for ac_prog in ginstall scoinst install; do
      for ac_exec_ext in '' $ac_executable_extensions; do
	if AS_EXECUTABLE_P(["$as_dir/$ac_prog$ac_exec_ext"]); then
	  if test $ac_prog = install &&
	    grep dspmsg "$as_dir/$ac_prog$ac_exec_ext" >/dev/null 2>&1; then
	    # AIX install.  It has an incompatible calling convention.
	    :
	  elif test $ac_prog = install &&
	    grep pwplus "$as_dir/$ac_prog$ac_exec_ext" >/dev/null 2>&1; then
	    # program-specific install script used by HP pwplus--don't use.
	    :
	  else
	    rm -rf conftest.one conftest.two conftest.dir
	    echo one > conftest.one
	    echo two > conftest.two
	    mkdir conftest.dir
	    if "$as_dir/$ac_prog$ac_exec_ext" -c conftest.one conftest.two "`pwd`/conftest.dir" &&
	      test -s conftest.one && test -s conftest.two &&
	      test -s conftest.dir/conftest.one &&
	      test -s conftest.dir/conftest.two
	    then
	      ac_cv_path_install="$as_dir/$ac_prog$ac_exec_ext -c"
	      break 3
	    fi
	  fi
	fi
      done
    done
    ;;
esac])
rm -rf conftest.one conftest.two conftest.dir
])dnl
  if test "${ac_cv_path_install+set}" = set; then
    INSTALL=$ac_cv_path_install
  else
    # As a last resort, use the slow shell script.  Don't cache a
    # value for INSTALL within a source directory, because that will
    # break other packages using the cache if that directory is
    # removed, or if the value is a relative name.
    INSTALL=$ac_install_sh
  fi
fi
dnl Do special magic for INSTALL instead of AC_SUBST, to get
dnl relative names right.
AC_MSG_RESULT([$INSTALL])

# Use test -z because SunOS4 sh mishandles braces in ${var-val}.
# It thinks the first close brace ends the variable substitution.
test -z "$INSTALL_PROGRAM" && INSTALL_PROGRAM='${INSTALL}'
AC_SUBST(INSTALL_PROGRAM)dnl

test -z "$INSTALL_SCRIPT" && INSTALL_SCRIPT='${INSTALL}'
AC_SUBST(INSTALL_SCRIPT)dnl

test -z "$INSTALL_DATA" && INSTALL_DATA='${INSTALL} -m 644'
AC_SUBST(INSTALL_DATA)dnl
])# AC_PROG_INSTALL
])
