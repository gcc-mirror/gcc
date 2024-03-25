# Configure script for c++tools
#   Copyright (C) 2020-2024 Free Software Foundation, Inc.
#   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# C++ has grown a C++20 mapper server.  This may be used to provide
# and/or learn and/or build required modules.  This sample server
# shows how the protocol introduced by wg21.link/p1184 may be used.
# By default g++ uses an in-process mapper.

sinclude(../config/acx.m4)
sinclude(../config/ax_lib_socket_nsl.m4)

AC_INIT(c++tools)

AC_CONFIG_SRCDIR([server.cc])

# Determine the noncanonical names used for directories.
ACX_NONCANONICAL_TARGET

AC_CANONICAL_SYSTEM
AC_PROG_INSTALL
test -z "$INSTALL_PROGRAM" && INSTALL_PROGRAM='${INSTALL}'
AC_SUBST(INSTALL_PROGRAM)

AC_PROG_CXX
MISSING=`cd $ac_aux_dir && ${PWDCMD-pwd}`/missing
AC_CHECK_PROGS([AUTOCONF], [autoconf], [$MISSING autoconf])
AC_CHECK_PROGS([AUTOHEADER], [autoheader], [$MISSING autoheader])

AC_LANG(C++)

dnl Enabled by default
AC_MSG_CHECKING([whether to build C++ tools])
  AC_ARG_ENABLE(c++-tools, 
    [AS_HELP_STRING([--enable-c++-tools],
		    [build auxiliary c++ tools])],
      cxx_aux_tools=$enableval,
      cxx_aux_tools=yes)

AC_MSG_RESULT($cxx_aux_tools)
CXX_AUX_TOOLS="$cxx_aux_tools"
AC_SUBST(CXX_AUX_TOOLS)

AC_ARG_ENABLE([maintainer-mode],
AS_HELP_STRING([--enable-maintainer-mode],
[enable maintainer mode.  Add rules to rebuild configurey bits]),,
[enable_maintainer_mode=no])
case "$enable_maintainer_mode" in
  yes) maintainer_mode=yes ;;
  no) maintainer_mode=no ;;
  *) AC_MSG_ERROR([unknown maintainer mode $enable_maintainer_mode]) ;;
esac
AC_MSG_CHECKING([maintainer-mode])
AC_MSG_RESULT([$maintainer_mode])
test "$maintainer_mode" = yes && MAINTAINER=yes
AC_SUBST(MAINTAINER)

# Handle configuration of checking; for the tools in this directory we
# default to release checking and stricter checks do not change this.

AC_ARG_ENABLE(checking,
[AS_HELP_STRING([[--enable-checking[=LIST]]],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: yes,no,all,none,release.])],
[ac_checking_flags="${enableval}"],[
# Default to checking.
ac_checking_flags=yes
])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes|all|release|assert) ac_assert_checking=1 ; ;;
	no|none) ac_assert_checking= ; ;;
	*) ;;
	esac
done
IFS="$ac_save_IFS"

if test x$ac_assert_checking != x ; then
  AC_DEFINE(ENABLE_ASSERT_CHECKING, 1,
[Define if you want assertions enabled.  This is a cheap check.])
fi

# Check whether --enable-default-pie was given.
AC_ARG_ENABLE(default-pie,
[AS_HELP_STRING([--enable-default-pie],
		  [enable Position Independent Executable as default])],
[PICFLAG=-fPIE], [PICFLAG=])

# Enable --enable-host-pie
AC_ARG_ENABLE(host-pie,
[AS_HELP_STRING([--enable-host-pie],
		[build host code as PIE])],
[PICFLAG=-fPIE; LD_PICFLAG=-pie], [])
AC_SUBST(PICFLAG)

# Enable --enable-host-bind-now
AC_ARG_ENABLE(host-bind-now,
[AS_HELP_STRING([--enable-host-bind-now],
       [link host code as BIND_NOW])],
[LD_PICFLAG="$LD_PICFLAG -Wl,-z,now"], [])
AC_SUBST(enable_host_bind_now)
AC_SUBST(LD_PICFLAG)

# Check if O_CLOEXEC is defined by fcntl
AC_CACHE_CHECK(for O_CLOEXEC, ac_cv_o_cloexec, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <fcntl.h>]], [[
return open ("/dev/null", O_RDONLY | O_CLOEXEC);]])],
[ac_cv_o_cloexec=yes],[ac_cv_o_cloexec=no])])
if test $ac_cv_o_cloexec = yes; then
  AC_DEFINE(HOST_HAS_O_CLOEXEC, 1,
  [Define if O_CLOEXEC supported by fcntl.])
fi

AC_CHECK_HEADERS(sys/mman.h)

# C++ Modules would like some networking features to provide the mapping
# server.  You can still use modules without them though.
# The following network-related checks could probably do with some
# Windows and other non-linux defenses and checking.

# Local socket connectivity wants AF_UNIX networking
# Check for AF_UNIX networking
AC_CACHE_CHECK(for AF_UNIX, ac_cv_af_unix, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>]],[[
sockaddr_un un;
un.sun_family = AF_UNSPEC;
int fd = socket (AF_UNIX, SOCK_STREAM, 0);
connect (fd, (sockaddr *)&un, sizeof (un));]])],
[ac_cv_af_unix=yes],
[ac_cv_af_unix=no])])
if test $ac_cv_af_unix = yes; then
  AC_DEFINE(HAVE_AF_UNIX, 1,
  [Define if AF_UNIX supported.])
fi

# Remote socket connectivity wants AF_INET6 networking
# Check for AF_INET6 networking
AC_CACHE_CHECK(for AF_INET6, ac_cv_af_inet6, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>]],[[
sockaddr_in6 in6;
in6.sin6_family = AF_UNSPEC;
struct addrinfo *addrs = 0;
struct addrinfo hints;
hints.ai_flags = 0;
hints.ai_family = AF_INET6;
hints.ai_socktype = SOCK_STREAM;
hints.ai_protocol = 0;
hints.ai_canonname = 0;
hints.ai_addr = 0;
hints.ai_next = 0;
int e = getaddrinfo ("localhost", 0, &hints, &addrs);
const char *str = gai_strerror (e);
freeaddrinfo (addrs);
int fd = socket (AF_INET6, SOCK_STREAM, 0);
connect (fd, (sockaddr *)&in6, sizeof (in6));]])],
[ac_cv_af_inet6=yes],
[ac_cv_af_inet6=no])])
if test $ac_cv_af_inet6 = yes; then
  AC_DEFINE(HAVE_AF_INET6, 1,
  [Define if AF_INET6 supported.])
fi

# Efficient server response wants epoll
# Check for epoll_create, epoll_ctl, epoll_pwait
AC_CACHE_CHECK(for epoll, ac_cv_epoll, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/epoll.h>]],[[
int fd = epoll_create (1);
epoll_event ev;
ev.events = EPOLLIN;
ev.data.fd = 0;
epoll_ctl (fd, EPOLL_CTL_ADD, 0, &ev);
epoll_pwait (fd, 0, 0, -1, 0);]])],
[ac_cv_epoll=yes],
[ac_cv_epoll=no])])
if test $ac_cv_epoll = yes; then
  AC_DEFINE(HAVE_EPOLL, 1,
  [Define if epoll_create, epoll_ctl, epoll_pwait provided.])
fi

# If we can't use epoll, try pselect.
# Check for pselect
AC_CACHE_CHECK(for pselect, ac_cv_pselect, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/select.h>]],[[
pselect (0, 0, 0, 0, 0, 0);]])],
[ac_cv_pselect=yes],
[ac_cv_pselect=no])])
if test $ac_cv_pselect = yes; then
  AC_DEFINE(HAVE_PSELECT, 1,
  [Define if pselect provided.])
fi

# And failing that, use good old select.
# If we can't even use this, the server is serialized.
# Check for select
AC_CACHE_CHECK(for select, ac_cv_select, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/select.h>]],[[
select (0, 0, 0, 0, 0);]])],
[ac_cv_select=yes],
[ac_cv_select=no])])
if test $ac_cv_select = yes; then
  AC_DEFINE(HAVE_SELECT, 1,
  [Define if select provided.])
fi

# Avoid some fnctl calls by using accept4, when available.
# Check for accept4
AC_CACHE_CHECK(for accept4, ac_cv_accept4, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/socket.h>]],[[
int err = accept4 (1, 0, 0, SOCK_NONBLOCK);]])],
[ac_cv_accept4=yes],
[ac_cv_accept4=no])])
if test $ac_cv_accept4 = yes; then
  AC_DEFINE(HAVE_ACCEPT4, 1,
  [Define if accept4 provided.])
fi

# For better server messages, look for a way to stringize network addresses
# Check for inet_ntop
AC_CACHE_CHECK(for inet_ntop, ac_cv_inet_ntop, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <arpa/inet.h>
#include <netinet/in.h>]],[[
sockaddr_in6 in6;
char buf[INET6_ADDRSTRLEN];
const char *str = inet_ntop (AF_INET6, &in6, buf, sizeof (buf));]])],
[ac_cv_inet_ntop=yes],
[ac_cv_inet_ntop=no])])
if test $ac_cv_inet_ntop = yes; then
  AC_DEFINE(HAVE_INET_NTOP, 1,
  [Define if inet_ntop provided.])
fi

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Solaris needs libsocket and libnsl for socket functions before 11.4.
# libcody uses those.
save_LIBS="$LIBS"
LIBS=
AX_LIB_SOCKET_NSL
NETLIBS="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(NETLIBS)

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
