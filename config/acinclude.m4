dnl This file is included into all any other acinclude file that needs
dnl to use these macros.

dnl This is copied from autoconf 2.12, but does calls our own AC_PROG_CC_WORKS,
dnl and doesn't call AC_PROG_CXX_GNU, cause we test for that in  AC_PROG_CC_WORKS.
dnl We are probably using a cross compiler, which will not be able to fully
dnl link an executable.  This should really be fixed in autoconf itself.
dnl Find a working G++ cross compiler. This only works for the GNU C++ compiler.
AC_DEFUN([CYG_AC_PROG_CXX_CROSS],
[AC_BEFORE([$0], [AC_PROG_CXXCPP])
AC_CHECK_PROGS(CXX, $CCC c++ g++ gcc CC cxx cc++, gcc)

CYG_AC_PROG_GXX_WORKS

if test $ac_cv_prog_gxx = yes; then
  GXX=yes
dnl Check whether -g works, even if CXXFLAGS is set, in case the package
dnl plays around with CXXFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS=
  AC_PROG_CXX_G
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  elif test $ac_cv_prog_cxx_g = yes; then
    CXXFLAGS="-g -O2"
  else
    CXXFLAGS="-O2"
  fi
else
  GXX=
  test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
fi
])

dnl See if the G++ compiler we found works.
AC_DEFUN([CYG_AC_PROG_GXX_WORKS],
[AC_MSG_CHECKING([whether the G++ compiler ($CXX $CXXFLAGS $LDFLAGS) actually works])
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
dnl Try a test case. We only compile, because it's close to impossible
dnl to get a correct fully linked executable with a cross compiler. For
dnl most cross compilers, this test is bogus. For G++, we can use various
dnl other compile line options to get a decent idea that the cross compiler
dnl actually does work, even though we can't produce an executable without
dnl more info about the target it's being compiled for. This only works
dnl for the GNU C++ compiler.

dnl Transform the name of the compiler to it's cross variant, unless
dnl CXX is set. This is also what CXX gets set to in the generated
dnl Makefile.
if test x"${CXX}" = xc++ ; then
    CXX=`echo gcc | sed -e "${program_transform_name}"`
fi

dnl Get G++'s full path to libgcc.a
libgccpath=`${CXX} --print-libgcc`

dnl If we don't have a path with libgcc.a on the end, this isn't G++.
if test `echo $libgccpath | sed -e 's:/.*/::'` = libgcc.a ; then
   ac_cv_prog_gxx=yes
else
   ac_cv_prog_gxx=no
fi

dnl If we are using G++, look for the files that need to exist if this
dnl compiler works.
if test x"${ac_cv_prog_gxx}" = xyes ; then
    gccfiles=`echo $libgccpath | sed -e 's:/libgcc.a::'`
    if test -f ${gccfiles}/specs -a -f ${gccfiles}/cpp -a -f ${gccfiles}/cc1plus; then
	gccfiles=yes
    else
	gccfiles=no
    fi
    gcclibs=`echo $libgccpath | sed -e 's:lib/gcc-lib/::' -e 's:/libgcc.a::' -e 's,\(.*\)/.*,\1,g'`/lib
    if test -d ${gcclibs}/ldscripts -a -f ${gcclibs}/libc.a -a -f ${gcclibs}/libstdc++.a ; then
	gcclibs=yes
    else
	gcclibs=no
    fi
fi

dnl If everything is OK, then we can safely assume the compiler works.
if test x"${gccfiles}" = xno -o x"${gcclibs}" = xno; then
    ac_cv_prog_cxx_works=no
    AC_MSG_ERROR(${CXX} is a non-working cross compiler)
else
   ac_cv_prog_cxx_works=yes 
fi

AC_LANG_RESTORE
AC_MSG_RESULT($ac_cv_prog_cxx_works)
if test x"$ac_cv_prog_cxx_works" = xno; then
  AC_MSG_ERROR([installation or configuration problem: C++ compiler cannot create executables.])
fi
AC_MSG_CHECKING([whether the G++ compiler ($CXX $CXXFLAGS $LDFLAGS) is a cross-compiler])
AC_MSG_RESULT($ac_cv_prog_cxx_cross)
cross_compiling=$ac_cv_prog_cxx_cross
AC_SUBST(CXX)
])

dnl ====================================================================
dnl Find a working GCC cross compiler. This only works for the GNU gcc compiler.
dnl This is based on the macros above for G++.
AC_DEFUN([CYG_AC_PROG_CC_CROSS],
[AC_BEFORE([$0], [AC_PROG_CCPP])
AC_CHECK_PROGS(CC, cc, gcc)

CYG_AC_PROG_GCC_WORKS

if test $ac_cv_prog_gcc = yes; then
  GCC=yes
dnl Check whether -g works, even if CFLAGS is set, in case the package
dnl plays around with CFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CFLAGS="${CFLAGS+set}"
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=
  AC_PROG_CC_G
  if test "$ac_test_CFLAGS" = set; then
    CFLAGS="$ac_save_CFLAGS"
  elif test $ac_cv_prog_cc_g = yes; then
    CFLAGS="-g -O2"
  else
    CFLAGS="-O2"
  fi
else
  GXX=
  test "${CFLAGS+set}" = set || CFLAGS="-g"
fi
])

dnl See if the GCC compiler we found works.
AC_DEFUN([CYG_AC_PROG_GCC_WORKS],
[AC_MSG_CHECKING([whether the Gcc compiler ($CC $CFLAGS $LDFLAGS) actually works])
AC_LANG_SAVE
AC_LANG_C
dnl Try a test case. We only compile, because it's close to impossible
dnl to get a correct fully linked executable with a cross
dnl compiler. For most cross compilers, this test is bogus. For G++,
dnl we can use various other compile line options to get a decent idea
dnl that the cross compiler actually does work, even though we can't
dnl produce an executable without more info about the target it's
dnl being compiled for. This only works for the GNU C++ compiler.

dnl Transform the name of the compiler to it's cross variant, unless
dnl CXX is set. This is also what CC gets set to in the generated Makefile.
if test x"${CC}" = xcc ; then
    CC=`echo gcc | sed -e "${program_transform_name}"`
fi

dnl Get Gcc's full path to libgcc.a
libgccpath=`${CC} --print-libgcc`

dnl If we don't have a path with libgcc.a on the end, this isn't G++.
if test `echo $libgccpath | sed -e 's:/.*/::'` = libgcc.a ; then
   ac_cv_prog_gcc=yes
else
   ac_cv_prog_gcc=no
fi

dnl If we are using Gcc, look for the files that need to exist if this
dnl compiler works.
if test x"${ac_cv_prog_gcc}" = xyes ; then
    gccfiles=`echo $libgccpath | sed -e 's:/libgcc.a::'`
    if test -f ${gccfiles}/specs -a -f ${gccfiles}/cpp -a -f ${gccfiles}/cc1plus; then
	gccfiles=yes
    else
	gccfiles=no
    fi
    gcclibs=`echo $libgccpath | sed -e 's:lib/gcc-lib/::' -e 's:/libgcc.a::' -e 's,\(.*\)/.*,\1,g'`/lib
    if test -d ${gcclibs}/ldscripts -a -f ${gcclibs}/libc.a -a -f ${gcclibs}/libstdc++.a ; then
	gcclibs=yes
    else
	gcclibs=no
    fi
fi

dnl If everything is OK, then we can safely assume the compiler works.
if test x"${gccfiles}" = xno -o x"${gcclibs}" = xno; then
    ac_cv_prog_cc_works=no
    AC_MSG_ERROR(${CC} is a non-working cross compiler)    
else
    ac_cv_prog_cc_works=yes
fi

AC_LANG_RESTORE
AC_MSG_RESULT($ac_cv_prog_cc_works)
if test x"$ac_cv_prog_cc_works" = xno; then
  AC_MSG_ERROR([installation or configuration problem: C++ compiler cannot create executables.])
fi
AC_MSG_CHECKING([whether the Gcc compiler ($CC $CFLAGS $LDFLAGS) is a cross-compiler])
AC_MSG_RESULT($ac_cv_prog_cc_cross)
cross_compiling=$ac_cv_prog_cc_cross
AC_SUBST(CC)
])

dnl ====================================================================
dnl Find the BFD library in the build tree. This is used to access and
dnl manipulate object or executable files.
AC_DEFUN([CYG_AC_PATH_BFD], [
AC_MSG_CHECKING(for the bfd header in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
dnl Look for the header file
AC_CACHE_VAL(ac_cv_c_bfdh,[
for i in $dirlist; do
    if test -f "$i/bfd/bfd.h" ; then
	ac_cv_c_bfdh=`(cd $i/bfd; ${PWDCMD-pwd})`
	break
    fi
done
])
if test x"${ac_cv_c_bfdh}" != x; then
    BFDHDIR="-I${ac_cv_c_bfdh}"
    AC_MSG_RESULT(${ac_cv_c_bfdh})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(BFDHDIR)

dnl Look for the library
AC_MSG_CHECKING(for the bfd library in the build tree)
AC_CACHE_VAL(ac_cv_c_bfdlib,[
for i in $dirlist; do
    if test -f "$i/bfd/Makefile" ; then
	ac_cv_c_bfdlib=`(cd $i/bfd; ${PWDCMD-pwd})`
    fi
done
])
dnl We list two directories cause bfd now uses libtool
if test x"${ac_cv_c_bfdlib}" != x; then
    BFDLIB="-L${ac_cv_c_bfdlib} -L${ac_cv_c_bfdlib}/.libs"
    AC_MSG_RESULT(${ac_cv_c_bfdlib})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(BFDLIB)
])

dnl ====================================================================
dnl Find the libiberty library. This defines many commonly used C
dnl functions that exists in various states based on the underlying OS.
AC_DEFUN([CYG_AC_PATH_LIBERTY], [
AC_MSG_CHECKING(for the liberty library in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_liberty,[
for i in $dirlist; do
    if test -f "$i/libiberty/Makefile" ; then
	ac_cv_c_liberty=`(cd $i/libiberty; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_liberty}" != x; then
    LIBERTY="-L${ac_cv_c_liberty}"
    AC_MSG_RESULT(${ac_cv_c_liberty})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(LIBERTY)
])

dnl ====================================================================
dnl Find the opcodes library. This is used to do dissasemblies.
AC_DEFUN([CYG_AC_PATH_OPCODES], [
AC_MSG_CHECKING(for the opcodes library in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_opc,[
for i in $dirlist; do
    if test -f "$i/opcodes/Makefile" ; then
	ac_cv_c_opc=`(cd $i/opcodes; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_opc}" != x; then
    OPCODESLIB="-L${ac_cv_c_opc}"
    AC_MSG_RESULT(${ac_cv_c_opc})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(OPCODESLIB)
])

dnl ====================================================================
dnl Look for the DejaGnu header file in the source tree. This file
dnl defines the functions used to testing support.
AC_DEFUN([CYG_AC_PATH_DEJAGNU], [
AC_MSG_CHECKING(for the testing support files in the source tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_dejagnu,[
for i in $dirlist; do
    if test -f "$srcdir/$i/ecc/ecc/infra/testlib/current/include/dejagnu.h" ; then
	ac_cv_c_dejagnu=`(cd $srcdir/$i/ecc/ecc/infra/testlib/current/include; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_dejagnu}" != x; then
    DEJAGNUHDIR="-I${ac_cv_c_dejagnu}"
    AC_MSG_RESULT(${ac_cv_c_dejagnu})
else
    AC_MSG_RESULT(none)
fi
AC_CACHE_VAL(ac_cv_c_dejagnulib,[
for i in $dirlist; do
    if test -f "$srcdir/$i/infra/testlib/current/lib/hostutil.exp" ; then
	ac_cv_c_dejagnulib=`(cd $srcdir/$i/infra/testlib/current/lib; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_dejagnulib}" != x; then
    DEJAGNULIB="${ac_cv_c_dejagnulib}"
else
    DEJAGNULIB=""
fi
AC_MSG_CHECKING(for runtest in the source tree)
AC_CACHE_VAL(ac_cv_c_runtest,[
for i in $dirlist; do
    if test -f "$srcdir/$i/dejagnu/runtest" ; then
	ac_cv_c_runtest=`(cd $srcdir/$i/dejagnu; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_runtest}" != x; then
    RUNTESTDIR="${ac_cv_c_runtest}"
   AC_MSG_RESULT(${ac_cv_c_runtest})
else
    RUNTESTDIR=""
    AC_MSG_RESULT(none)
fi
AC_SUBST(RUNTESTDIR)
AC_SUBST(DEJAGNULIB)
AC_SUBST(DEJAGNUHDIR)
])

dnl ====================================================================
dnl Find the libintl library in the build tree. This is for
dnl  internationalization support.
AC_DEFUN([CYG_AC_PATH_INTL], [
AC_MSG_CHECKING(for the intl header in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
dnl Look for the header file
AC_CACHE_VAL(ac_cv_c_intlh,[
for i in $dirlist; do
    if test -f "$i/intl/libintl.h" ; then
	ac_cv_c_intlh=`(cd $i/intl; ${PWDCMD-pwd})`
	break
    fi
done
])
if test x"${ac_cv_c_intlh}" != x; then
    INTLHDIR="-I${ac_cv_c_intlh}"
    AC_MSG_RESULT(${ac_cv_c_intlh})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(INTLHDIR)

dnl Look for the library
AC_MSG_CHECKING(for the libintl library in the build tree)
AC_CACHE_VAL(ac_cv_c_intllib,[
for i in $dirlist; do
    if test -f "$i/intl/Makefile" ; then
	ac_cv_c_intllib=`(cd $i/intl; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_intllib}" != x; then
    INTLLIB="-L${ac_cv_c_intllib} -lintl"
    AC_MSG_RESULT(${ac_cv_c_intllib})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(INTLLIB)
])

dnl ====================================================================
dnl Find the libiberty library.
AC_DEFUN([CYG_AC_PATH_LIBIBERTY], [
AC_MSG_CHECKING(for the libiberty library in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_libib,[
for i in $dirlist; do
    if test -f "$i/libiberty/Makefile" ; then
	ac_cv_c_libib=`(cd $i/libiberty/; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_libib}" != x; then
    LIBIBERTY="-L${ac_cv_c_libib}"
    AC_MSG_RESULT(${ac_cv_c_libib})
else
    AC_MSG_RESULT(none)
fi
AC_SUBST(LIBIBERTY)
])

dnl ====================================================================
dnl Find all the ILU headers and libraries
AC_DEFUN([CYG_AC_PATH_ILU], [
AC_MSG_CHECKING(for ILU kernel headers in the source tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_iluh,[
for i in $dirlist; do
    if test -f "${srcdir}/$i/ilu/runtime/kernel/method.h" ; then
	ac_cv_c_iluh=`(cd ${srcdir}/$i/ilu/runtime/kernel; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_iluh}" != x; then
    ILUHDIR="-I${ac_cv_c_iluh}"
    AC_MSG_RESULT(${ac_cv_c_iluh})
else
    AC_MSG_RESULT(none)
fi

AC_MSG_CHECKING(for ILU kernel headers in the build tree)
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
AC_CACHE_VAL(ac_cv_c_iluh5,[
for i in $dirlist; do
    if test -f "$i/ilu/runtime/kernel/iluconf.h" ; then
	ac_cv_c_iluh5=`(cd $i/ilu/runtime/kernel; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_iluh5}" != x; then
    ILUHDIR="${ILUHDIR} -I${ac_cv_c_iluh5}"
    AC_MSG_RESULT(${ac_cv_c_iluh5})
else
    AC_MSG_RESULT(none)
fi

AC_MSG_CHECKING(for ILU C++ headers in the source tree)
AC_CACHE_VAL(ac_cv_c_iluh2,[
for i in $dirlist; do
    if test -f "${srcdir}/$i/ilu/stubbers/cpp/resource.h" ; then
	ac_cv_c_iluh2=`(cd ${srcdir}/$i/ilu/stubbers/cpp; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_iluh2}" != x; then
    ILUHDIR="${ILUHDIR} -I${ac_cv_c_iluh2}"
    AC_MSG_RESULT(${ac_cv_c_iluh2})
else
    AC_MSG_RESULT(none)
fi

AC_MSG_CHECKING(for ILU C headers)
AC_CACHE_VAL(ac_cv_c_iluh3,[
for i in $dirlist; do
    if test -f "${srcdir}/$i/ilu/stubbers/c/resource.h" ; then
	ac_cv_c_iluh3=`(cd ${srcdir}/$i/ilu/stubbers/c  ; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_iluh3}" != x; then
    ILUHDIR="${ILUHDIR} -I${ac_cv_c_iluh3}"
    AC_MSG_RESULT(${ac_cv_c_iluh3})
else
    AC_MSG_RESULT(none)
fi

AC_MSG_CHECKING(for ILU C runtime headers)
AC_CACHE_VAL(ac_cv_c_iluh4,[
for i in $dirlist; do
    if test -f "${srcdir}/$i/ilu/runtime/c/ilucstub.h" ; then
	ac_cv_c_iluh4=`(cd ${srcdir}/$i/ilu/runtime/c  ; ${PWDCMD-pwd})`
    fi
done
])
if test x"${ac_cv_c_iluh4}" != x; then
    ILUHDIR="${ILUHDIR} -I${ac_cv_c_iluh4}"
    AC_MSG_RESULT(${ac_cv_c_iluh4})
else
    AC_MSG_RESULT(none)
fi

AC_CACHE_VAL(ac_cv_c_ilupath,[
for i in $dirlist; do
    if test -f "$i/ilu/Makefile" ; then
	ac_cv_c_ilupath=`(cd $i/ilu; ${PWDCMD-pwd})`
	break
    fi
done
])
ILUTOP=${ac_cv_c_ilupath}

AC_MSG_CHECKING(for the ILU library in the build tree)
AC_CACHE_VAL(ac_cv_c_ilulib,[
if test -f "$ac_cv_c_ilupath/runtime/kernel/Makefile" ; then
    ac_cv_c_ilulib=`(cd $ac_cv_c_ilupath/runtime/kernel; ${PWDCMD-pwd})`
    AC_MSG_RESULT(found ${ac_cv_c_ilulib}/libilu.a)
else
    AC_MSG_RESULT(no)
fi])
   
AC_MSG_CHECKING(for the ILU C++ bindings library in the build tree)
AC_CACHE_VAL(ac_cv_c_ilulib2,[
if test -f "$ac_cv_c_ilupath/runtime/cpp/Makefile" ; then
    ac_cv_c_ilulib2=`(cd $ac_cv_c_ilupath/runtime/cpp; ${PWDCMD-pwd})`
    AC_MSG_RESULT(found ${ac_cv_c_ilulib2}/libilu-c++.a)
else
    AC_MSG_RESULT(no)
fi])

AC_MSG_CHECKING(for the ILU C bindings library in the build tree)
AC_CACHE_VAL(ac_cv_c_ilulib3,[
if test -f "$ac_cv_c_ilupath/runtime/c/Makefile" ; then
    ac_cv_c_ilulib3=`(cd $ac_cv_c_ilupath/runtime/c; ${PWDCMD-pwd})`
    AC_MSG_RESULT(found ${ac_cv_c_ilulib3}/libilu-c.a)
else
    AC_MSG_RESULT(no)
fi])

AC_MSG_CHECKING(for the ILU Tk bindings library in the build tree)
AC_CACHE_VAL(ac_cv_c_ilulib4,[
if test -f "$ac_cv_c_ilupath/runtime/mainloop/Makefile" ; then
    ac_cv_c_ilulib4=`(cd $ac_cv_c_ilupath/runtime/mainloop; ${PWDCMD-pwd})`
    AC_MSG_RESULT(found ${ac_cv_c_ilulib4}/libilu-tk.a)
else
    AC_MSG_RESULT(no)
fi])

if test x"${ac_cv_c_ilulib}" = x -a x"${ac_cv_c_ilulib2}" = x; then
  ILUHDIR=""
fi

if test x"${ac_cv_c_ilulib}" != x -a x"${ac_cv_c_ilulib2}" != x; then
    ILULIB="-L${ac_cv_c_ilulib} -L${ac_cv_c_ilulib2} -L${ac_cv_c_ilulib3} -L${ac_cv_c_ilulib4}"
else
    ILULIB=""
fi

if test x"${ILULIB}" = x; then
    AC_MSG_CHECKING(for ILU libraries installed with the compiler)
    AC_CACHE_VAL(ac_cv_c_ilulib5,[
    NATIVE_GCC=`echo gcc | sed -e "${program_transform_name}"`

    dnl Get G++'s full path to it's libraries
    ac_cv_c_ilulib5=`${NATIVE_GCC} --print-libgcc | sed -e 's:lib/gcc-lib/.*::'`lib
    if test -f $ac_cv_c_ilulib5/libilu-c.a -o -f $ac_cv_c_ilulib5/libilu-c.so ; then
        if test x"${ILUHDIR}" = x; then
               ILUHDIR="-I${ac_cv_c_ilulib5}/../include"
        fi
        ILULIB="-L${ac_cv_c_ilulib5}"
        AC_MSG_RESULT(${ac_cv_c_ilulib5})
    else
        ac_cv_c_ilulib=none
        AC_MSG_RESULT(none)
    fi
fi])
AC_SUBST(ILUHDIR)
AC_SUBST(ILULIB)
AC_SUBST(ILUTOP)
])

dnl ====================================================================
dnl This defines the byte order for the host. We can't use
dnl AC_C_BIGENDIAN, cause we want to create a config file and
dnl substitue the real value, so the header files work right
AC_DEFUN([CYG_AC_C_ENDIAN], [
AC_MSG_CHECKING(to see if this is a little endian host)
AC_CACHE_VAL(ac_cv_c_little_endian, [
ac_cv_c_little_endian=unknown
# See if sys/param.h defines the BYTE_ORDER macro.
AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/param.h>], [
#if !BYTE_ORDER || !_BIG_ENDIAN || !_LITTLE_ENDIAN
 bogus endian macros
#endif], [# It does; now see whether it defined to _LITTLE_ENDIAN or not.
AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/param.h>], [
#if BYTE_ORDER != _LITTLE_ENDIAN
 not big endian
#endif], ac_cv_c_little_endian=yes, ac_cv_c_little_endian=no)
])
if test ${ac_cv_c_little_endian} = unknown; then
old_cflags=$CFLAGS
CFLAGS=-g
AC_TRY_RUN([
main () {
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  exit (u.c[0] == 1);
}],
ac_cv_c_little_endian=no,
ac_cv_c_little_endian=yes,[
dnl Yes, this is ugly, and only used for a canadian cross anyway. This
dnl is just to keep configure from stopping here.
case "${host}" in
changequote(,)
   i[3456789]86-*-*) ac_cv_c_little_endian=yes ;;
   sparc*-*-*)    ac_cv_c_little_endian=no ;;
changequote([,])
  *)    AC_MSG_WARN(Can't cross compile this test) ;;
esac])
CFLAGS=$old_cflags
fi])

if test x"${ac_cv_c_little_endian}" = xyes; then
    AC_DEFINE(LITTLE_ENDIAN_HOST)
    ENDIAN="CYG_LSBFIRST";
else
    ENDIAN="CYG_MSBFIRST";
fi
AC_MSG_RESULT(${ac_cv_c_little_endian})
AC_SUBST(ENDIAN)
])

dnl ====================================================================
dnl Look for the path to libgcc, so we can use it to directly link
dnl in libgcc.a with LD.
AC_DEFUN([CYG_AC_PATH_LIBGCC],
[AC_MSG_CHECKING([Looking for the path to libgcc.a])
AC_LANG_SAVE
AC_LANG_C

dnl Get Gcc's full path to libgcc.a
libgccpath=`${CC} --print-libgcc`

dnl If we don't have a path with libgcc.a on the end, this isn't G++.
if test `echo $libgccpath | sed -e 's:/.*/::'` = libgcc.a ; then
   ac_cv_prog_gcc=yes
else
   ac_cv_prog_gcc=no
fi

dnl 
if test x"${ac_cv_prog_gcc}" = xyes ; then
   gccpath=`echo $libgccpath | sed -e 's:/libgcc.a::'`
   LIBGCC="-L${gccpath}"
   AC_MSG_RESULT(${gccpath})
else
   LIBGCC=""
   AC_MSG_ERROR(Not using gcc)
fi

AC_LANG_RESTORE
AC_SUBST(LIBGCC)
])
