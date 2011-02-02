# Copyright (c) 1999, 2000, 2001, 2002, 2003, 2006, 2010, 2011 by Red Hat, Inc.
# All rights reserved.
# Copyright 2004 Nathanael Nerode
# 
# THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
# OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
# 
# Permission is hereby granted to use or copy this program
# for any purpose,  provided the above notices are retained on all copies.
# Permission to modify the code and to distribute modified code is granted,
# provided the above notices are retained, and a notice that the code was
# modified is included with the above copyright notice.
#
# Original author: Tom Tromey
# Modified by Nathanael Nerode

dnl Process this file with autoconf to produce configure.

AC_PREREQ(2.64)
AC_INIT
AC_CONFIG_SRCDIR(gcj_mlc.c)
# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Get the 'noncanonical' system names.
ACX_NONCANONICAL_TARGET

# This works around an automake problem.
mkinstalldirs="`cd $ac_aux_dir && ${PWDCMD-pwd}`/mkinstalldirs"
AC_SUBST(mkinstalldirs)

AM_INIT_AUTOMAKE(gc, 6.6, no-define)

# The autoconf 2.5x version of the no-executables hack.
GCC_NO_EXECUTABLES

# Yak.  We must force CC and CXX to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS and CXXFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AM_PROG_CC_C_O

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)

# Newer automakes demand CCAS and CCASFLAGS.
: ${CCAS='$(CC)'}
: ${CCASFLAGS='$(CFLAGS)'}
AC_SUBST(CCAS)
AC_SUBST(CCASFLAGS)

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

. ${srcdir}/configure.host

case ${gc_basedir} in
/* | [A-Za-z]:[/\\]*) gc_flagbasedir=${gc_basedir} ;;
*) gc_flagbasedir='$(top_builddir)/'${gc_basedir} ;;
esac

gc_cflags="${gc_cflags} -Iinclude -I"'$(top_builddir)'"/./targ-include -I${gc_flagbasedir}/libc/include"
case "${host}" in
  *-*-cygwin32*)
    gc_cflags="${gc_cflags} -I${gc_flagbasedir}/../winsup/include"
    ;;
esac

dnl Add for mingw targets GC_BUILD option
case "${host}" in
  *-*-mingw*)
    gc_cflags="${gc_cflags} -DGC_BUILD=1"
    ;;
esac

dnl gc_cflags="${gc_cflags} -fno-builtin"

GC_CFLAGS=${gc_cflags}
AC_SUBST(GC_CFLAGS)

AC_PROG_LIBTOOL

dnl We use these options to decide which functions to include.
AC_ARG_WITH(target-subdir,
[  --with-target-subdir=SUBDIR
                          configuring with a cross compiler])
AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST  configuring with a cross compiler])

AC_MSG_CHECKING([for thread model used by GCC])
THREADS=`$CC -v 2>&1 | sed -n 's/^Thread model: //p'`
if test -z "$THREADS"; then
   THREADS=no
fi
AC_MSG_RESULT([$THREADS])

AC_ARG_ENABLE(parallel-mark,
[  --enable-parallel-mark	parallelize marking and free list construction],
   [case "$THREADS" in
      no | none | single)
	AC_MSG_ERROR([Parallel mark requires --enable-threads=x spec])
	;;
    esac]
)

AM_CPPFLAGS="-I`cd $srcdir && ${PWDCMD-pwd}`/include"
THREADLIBS=
case "$THREADS" in
 no | none | single)
    THREADS=none
    ;;
 posix | posix95 | pthreads)
    THREADS=posix
    THREADLIBS=-lpthread
    case "$host" in
     x86-*-linux* | ia64-*-linux* | i586-*-linux* | i686-*-linux* | x86_64-*-linux* | alpha-*-linux*)
	AC_DEFINE(GC_LINUX_THREADS,1,[support for Xavier Leroy's Linux threads])
	AC_DEFINE(_REENTRANT,1,[Use reentrant code])
        if test "${enable_parallel_mark}" = yes; then
	  AC_DEFINE(PARALLEL_MARK,1,[allow the marker to run in multiple threads])
	fi
	AC_DEFINE(THREAD_LOCAL_ALLOC,1,[define GC_local_malloc() & GC_local_malloc_atomic()])
	;;
     *-*-linux*)
	AC_DEFINE(GC_LINUX_THREADS,1)
	AC_DEFINE(_REENTRANT,1)
	;;
     *-*-aix*)
	AC_DEFINE(GC_AIX_THREADS,1,[support AIX threads])
	AC_DEFINE(_REENTRANT,1)
	;;
     *-*-hpux11*)
	AC_MSG_WARN("Only HP-UX 11 POSIX threads are supported.")
	AC_DEFINE(GC_HPUX_THREADS,1,[enables support for HP/UX 11 pthreads])
	AC_DEFINE(_POSIX_C_SOURCE,199506L,[POSIX version of C Source])
	if test "${enable_parallel_mark}" = yes; then
	  AC_DEFINE(PARALLEL_MARK,1)
	fi
	AC_DEFINE(THREAD_LOCAL_ALLOC,1)
	THREADLIBS="-lpthread -lrt"
	# HPUX needs REENTRANT for the _r calls.
	AC_DEFINE(_REENTRANT, 1, [Required define if using POSIX threads])
	;;
     *-*-hpux10*)
	AC_MSG_WARN("Only HP-UX 11 POSIX threads are supported.")
	;;
      *-*-kfreebsd*-gnu)
       AC_DEFINE(GC_FREEBSD_THREADS)
       INCLUDES="$INCLUDES -pthread"
       THREADDLLIBS=-pthread
       AC_DEFINE(_REENTRANT)
         if test "${enable_parallel_mark}" = yes; then
         AC_DEFINE(PARALLEL_MARK)
       fi
       AC_DEFINE(THREAD_LOCAL_ALLOC)
       AC_DEFINE(USE_COMPILER_TLS, 1,[use tls for boehm])
               ;;
     *-*-freebsd*)
	AC_MSG_WARN("FreeBSD does not yet fully support threads with Boehm GC.")
	AC_DEFINE(GC_FREEBSD_THREADS,1,[support FreeBSD threads])
	AM_CPPFLAGS="$AM_CPPFLAGS -pthread"
	THREADLIBS=-pthread
      	;;
     *-*-solaris2.8*)
	AC_DEFINE(GC_SOLARIS_PTHREADS,1,[support for Solaris pthreads])
	# Need to use alternate thread library, otherwise gctest hangs
	# on Solaris 8.
	multi_os_directory=`$CC -print-multi-os-directory`
	THREADLIBS="-L/usr/lib/lwp/$multi_os_directory \
		    -R/usr/lib/lwp/$multi_os_directory -lpthread -lthread -lrt"
	;;
     *-*-solaris2*)
	AC_DEFINE(GC_SOLARIS_PTHREADS,1,[support for Solaris pthreads])
	# The alternate thread library was only introduced in Solaris 8 and
	# became the default in Solaris 9, so no need for the special code
	# above otherwise.
	# nanosleep, sched_yield, and sem_* only live in librt before
	# Solaris 11.
	THREADLIBS="-lpthread -lrt"
	;;
     *-*-irix*)
	AC_DEFINE(GC_IRIX_THREADS,1,[support for Irix pthreads])
	;;
     *-*-cygwin*)
	AC_DEFINE(GC_WIN32_THREADS,1,[support for win32 threads])
	;;
     *-*-darwin*)
	AC_DEFINE(GC_DARWIN_THREADS,1,[support for Mac OS X pthreads])
	AC_DEFINE(THREAD_LOCAL_ALLOC,1)
	if test "${enable_parallel_mark}" = yes; then
	  AC_DEFINE(PARALLEL_MARK,1)
	fi
	;;
     *-*-osf*)
	AC_DEFINE(GC_OSF1_THREADS,1,[support for Tru64 pthreads])
        if test "${enable_parallel_mark}" = yes; then
	  AC_DEFINE(PARALLEL_MARK,1)
	  AC_DEFINE(THREAD_LOCAL_ALLOC,1)
	  # May want to enable it in other cases, too.
	  # Measurements havent yet been done.
	fi
	AM_CPPFLAGS="$AM_CPPFLAGS -pthread"
	THREADLIBS="-lpthread -lrt"
	;;
    esac
    ;;
 win32)
    AC_DEFINE(GC_WIN32_THREADS,1)
    dnl Old wine getenv may not return NULL for missing entry.
    dnl Define EMPTY_GETENV_RESULTS here to work around the bug.
    ;;
 dgux386)
    THREADS=dgux386
AC_MSG_RESULT($THREADLIBS)
    # Use pthread GCC  switch
    THREADLIBS=-pthread
    if test "${enable_parallel_mark}" = yes; then
        AC_DEFINE(PARALLEL_MARK,1)
    fi
    AC_DEFINE(THREAD_LOCAL_ALLOC,1)
    AC_DEFINE(GC_DGUX386_THREADS,1,[support for DB/UX on I386 threads])
    AC_DEFINE(DGUX_THREADS,1,[No description])
    # Enable _POSIX4A_DRAFT10_SOURCE with flag -pthread
    AM_CPPFLAGS="-pthread $AM_CPPFLAGS"
    ;;
 aix)
    THREADS=posix
    THREADLIBS=-lpthread
    AC_DEFINE(GC_AIX_THREADS,1)
    AC_DEFINE(_REENTRANT,1)
    ;;
 decosf1 | irix | mach | os2 | solaris | dce | vxworks)
    AC_MSG_ERROR(thread package $THREADS not yet supported)
    ;;
 *)
    AC_MSG_ERROR($THREADS is an unknown thread package)
    ;;
esac
AC_SUBST(THREADLIBS)

case "$host" in
   powerpc-*-darwin*)
      powerpc_darwin=true
      ;;
esac
AM_CONDITIONAL(POWERPC_DARWIN,test x$powerpc_darwin = xtrue)

# Darwin needs a few extra special tests to deal with variation in the
# system headers.
case "$host" in
  powerpc*-*-darwin*)
    AC_CHECK_MEMBER(ppc_thread_state_t.r0,
      AC_DEFINE(HAS_PPC_THREAD_STATE_R0,1,
	[ppc_thread_state_t has field r0]),,
      [#include <mach/thread_status.h>])
    AC_CHECK_MEMBER(ppc_thread_state_t.__r0,
      AC_DEFINE(HAS_PPC_THREAD_STATE___R0,1,dnl
	[ppc_thread_state_t has field __r0]),,
      [#include <mach/thread_status.h>])
    AC_CHECK_MEMBER(ppc_thread_state64_t.r0,
      AC_DEFINE(HAS_PPC_THREAD_STATE64_R0,1,dnl
	[ppc_thread_state64_t has field r0]),,
      [#include <mach/thread_status.h>])
    AC_CHECK_MEMBER(ppc_thread_state64_t.__r0,
      AC_DEFINE(HAS_PPC_THREAD_STATE64___R0,1,dnl
	[ppc_thread_state64_t has field __r0]),,
      [#include <mach/thread_status.h>])
    ;;
  i?86*-*-darwin*)
    AC_CHECK_MEMBER(x86_thread_state32_t.eax,
      AC_DEFINE(HAS_X86_THREAD_STATE32_EAX,1,dnl
	[x86_thread_state32_t has field eax]),,
      [#include <sys/cdefs.h>
      #include <mach/thread_status.h>])
    AC_CHECK_MEMBER(x86_thread_state32_t.__eax,
      AC_DEFINE(HAS_X86_THREAD_STATE32___EAX,1,dnl
	[x86_thread_state32_t has field __eax]),,
      [#include <sys/cdefs.h>
      #include <mach/thread_status.h>])
    ;;
  x86_64-*-darwin*)
    AC_CHECK_MEMBER(x86_thread_state64_t.rax,
      AC_DEFINE(HAS_X86_THREAD_STATE64_RAX,1,dnl
	[x86_thread_state64_t has field rax]),,
      [#include <sys/cdefs.h>
      #include <mach/thread_status.h>])
    AC_CHECK_MEMBER(x86_thread_state64_t.__rax,
      AC_DEFINE(HAS_X86_THREAD_STATE64___RAX,1,dnl
	[x86_thread_state64_t has field __rax]),,
      [#include <sys/cdefs.h>
      #include <mach/thread_status.h>])
     ;;
  *) ;;
esac

case "$host" in
  # While IRIX 6 has libdl for the O32 and N32 ABIs, it's missing for N64
  # and unnecessary everywhere.
  mips-sgi-irix6*) ;;
  # We never want libdl on darwin. It is a fake libdl that just ends up making
  # dyld calls anyway
  *-*-darwin*) ;;
  *)
    AC_CHECK_LIB(dl, dlopen, EXTRA_TEST_LIBS="$EXTRA_TEST_LIBS -ldl")
    ;;
esac

# extra LD Flags which are required for targets
case "${host}" in
  *-*-darwin*)
    extra_ldflags_libgc=-Wl,-single_module
    ;;
esac
AC_SUBST(extra_ldflags_libgc)

AC_SUBST(EXTRA_TEST_LIBS)

target_all=libgcjgc.la
AC_SUBST(target_all)

dnl If the target is an eCos system, use the appropriate eCos
dnl I/O routines.
dnl FIXME: this should not be a local option but a global target
dnl system; at present there is no eCos target.
TARGET_ECOS="no"
AC_ARG_WITH(ecos,
[  --with-ecos             enable runtime eCos target support],
TARGET_ECOS="$with_ecos"
)

addobjs=
addlibs=
addincludes=
addtests=
case "$TARGET_ECOS" in
   no)
      ;;
   *)
      AC_DEFINE(ECOS,1,[Target is ECOS])
      AM_CPPFLAGS="${AM_CPPFLAGS} -I${TARGET_ECOS}/include"
      addobjs="$addobjs ecos.lo"
      ;;
esac

if test "${enable_cplusplus}" = yes; then
      addincludes="$addincludes include/gc_cpp.h include/gc_allocator.h"
      addtests="$addtests test_cpp"
fi

AM_CONDITIONAL(CPLUSPLUS, test "${enable_cplusplus}" = yes)

AC_SUBST(CXX)

AC_SUBST(AM_CPPFLAGS)

# Configuration of shared libraries
#
AC_MSG_CHECKING(whether to build shared libraries)
AC_ENABLE_SHARED

case "$host" in
 alpha-*-openbsd*)
     enable_shared=no
     AC_MSG_RESULT(no)
     ;;
 *)
     AC_MSG_RESULT(yes)
     ;;
esac

# Checks for pthreads functions
#
oldLIBS="$LIBS"
LIBS="$LIBS $THREADLIBS"
AC_CHECK_FUNCS([pthread_getattr_np])
LIBS="$oldLIBS"

# Configuration of machine-dependent code
#
# Set NO_EXECUTE_PERMISSION by default because gcj already uses
# ffi_closure_{alloc,free} which takes care of allocating trampolines
# in executable memory.
#
AC_MSG_CHECKING(which machine-dependent code should be used)
machdep=
case "$host" in
 alpha*-*-openbsd*)
    machdep="alpha_mach_dep.lo"
    if test x"${ac_cv_lib_dl_dlopen}" != xyes ; then
       AC_MSG_WARN(OpenBSD/Alpha without dlopen(). Shared library support is disabled)
    fi
    ;;
 alpha*-*-linux*)
    machdep="alpha_mach_dep.lo"
    ;;
 i?86-*-solaris2.[[89]] | i?86-*-solaris2.1?)
    AC_DEFINE(SOLARIS25_PROC_VDB_BUG_FIXED,1,[PROC_VDB in Solaris 2.5 gives wrong values for dirty bits])
    ;;
 mipstx39-*-elf*)
    machdep="mips_ultrix_mach_dep.lo"
    AC_DEFINE(STACKBASE, __stackbase,[No description])
    AC_DEFINE(DATASTART_IS_ETEXT,1,[No description])
    ;;
 mips-dec-ultrix*)
    machdep="mips_ultrix_mach-dep.lo"
    ;;
 mips-nec-sysv*|mips-unknown-sysv*)
    ;;
 mips*-*-linux*)
    ;;
 mips-*-*)
    machdep="mips_sgi_mach_dep.lo"
    ;;
 sparc-*-netbsd*)
    machdep="sparc_netbsd_mach_dep.lo"
    ;;
 sparc-sun-solaris2.3)
    machdep="sparc_mach_dep.lo"
    AC_DEFINE(SUNOS53_SHARED_LIB,1,[Avoid Solaris 5.3 dynamic library bug])
    ;;
 sparc*-sun-solaris2.*)
    machdep="sparc_mach_dep.lo"
    ;;
 ia64-*-*)
    machdep="mach_dep.lo ia64_save_regs_in_stack.lo"
    ;;
esac
AC_DEFINE(NO_EXECUTE_PERMISSION,1,[cause some or all of the heap to not have execute permission])
if test x"$machdep" = x; then
AC_MSG_RESULT($machdep)
   machdep="mach_dep.lo"
fi
addobjs="$addobjs $machdep"
AC_SUBST(addobjs)
AC_SUBST(addincludes)
AC_SUBST(addlibs)
AC_SUBST(addtests)

#
# Check for AViiON Machines running DGUX
#
ac_is_dgux=no
AC_CHECK_HEADER(sys/dg_sys_info.h, [ac_is_dgux=yes])

AC_MSG_CHECKING(if host is AViiON running DGUX)
AC_MSG_RESULT($ac_is_dgux)
    ## :GOTCHA: we do not check anything but sys/dg_sys_info.h
if test $ac_is_dgux = yes; then
    if test "$enable_full_debug" = "yes"; then
        GC_CFLAGS="$GC_CFLAGS -mstandard"
    fi
    GC_CFLAGS="$GC_CFLAGS -DDGUX -D_DGUX_SOURCE -Di386 -mno-legend"
fi

dnl As of 4.13a2, the collector will not properly work on Solaris when
dnl built with gcc and -O.  So we remove -O in the appropriate case.
dnl Not needed anymore on Solaris.
AC_MSG_CHECKING([whether GCC optimization should be disabled])
O0_CFLAGS=
case "$host" in
 *aix*) test "$GCC" = yes && O0_CFLAGS=-O0 ;;
 *) ;;
esac
if test x"$O0_CFLAGS" != x; then
   AC_MSG_RESULT(yes)
else
   AC_MSG_RESULT(no)
fi
AC_SUBST([O0_CFLAGS])

dnl Include defines that have become de facto standard.
dnl ALL_INTERIOR_POINTERS can be overridden in startup code.
AC_DEFINE(SILENT,1,[disables statistics printing])
AC_DEFINE(NO_SIGNALS,1,[does not disable signals])
AC_DEFINE(ALL_INTERIOR_POINTERS,1,[allows all pointers to the interior of objects to be recognized])

dnl By default, make the library as general as possible.
AC_DEFINE(JAVA_FINALIZATION,1,[make it somewhat safer to finalize objects out of order])
AC_DEFINE(GC_GCJ_SUPPORT,1,[include support for gcj])
AC_DEFINE(ATOMIC_UNCOLLECTABLE,1,[include code for GC_malloc_atomic_uncollectable])

AC_ARG_ENABLE(gc-debug,
[  --enable-gc-debug	include full support for pointer backtracing etc.],
[ if test "$enable_gc_debug" = "yes"; then
    AC_MSG_WARN("Must define GC_DEBUG and use debug alloc. in clients.")
    AC_DEFINE(KEEP_BACK_PTRS,1,[Add code to save back pointers])
    AC_DEFINE(DBG_HDRS_ALL,1,[Make sure that all objects have debug headers])
    case $host in
      ia64-*-linux* )
	AC_DEFINE(MAKE_BACK_GRAPH,1,[Enable GC_PRINT_BACK_HEIGHT environment variable])
      ;;
      x86-*-linux* | i586-*-linux* | i686-*-linux* | x86_64-*-linux* )
	AC_DEFINE(MAKE_BACK_GRAPH,1)
	AC_MSG_WARN("Client must not use -fomit-frame-pointer.")
	AC_DEFINE(SAVE_CALL_COUNT, 8, [number of call frames saved with objects allocated through the debugging interface])
      ;;
      i[3456]86-*-dgux*)
	AC_DEFINE(MAKE_BACK_GRAPH,1)
      ;;
    esac 
  fi])

if test "${gc_use_mmap}" = "yes"; then
  AC_DEFINE(USE_MMAP, 1, [use MMAP instead of sbrk to get new memory])
fi

if test -n "$with_cross_host" &&
   test x"$with_cross_host" != x"no"; then
  toolexecdir='$(exec_prefix)/$(target_noncanonical)'
  toolexeclibdir='$(toolexecdir)/lib'
else
  toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
  toolexeclibdir='$(libdir)'
fi
multi_os_directory=`$CC -print-multi-os-directory`
case $multi_os_directory in
  .) ;; # Avoid trailing /.
  *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_HEADERS([include/gc_config.h include/gc_ext_config.h])

AC_CONFIG_FILES(Makefile include/Makefile threads.mk)
AC_OUTPUT
