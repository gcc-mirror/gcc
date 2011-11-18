/* Copyright (C) 2003, 2006, 2008, 2009, 2011 Free Software Foundation, Inc.
   Test builtin preprocessor assertions.
   By Kaveh Ghazi <ghazi@caip.rutgers.edu>.  */

/* { dg-do preprocess } */
/* { dg-options "-ansi -Wno-deprecated" } */

/* Check for #system assertions.  */

#if defined __linux__
# if !#system(linux) || !#system(unix) || !#system(posix)
#  error
# endif
#elif #system(linux)
# error
#endif

#if defined __gnu_hurd__
# if !#system(gnu) || !#system(unix) || !#system(posix) || !#system(mach)
#  error
# endif
#elif #system(gnu)
# error
#endif

#if defined __FreeBSD__
# if !#system(FreeBSD) || !#system(unix) || !#system(bsd)
#  error
# endif
#elif #system(FreeBSD)
# error
#endif

#if defined __NetBSD__
# if !#system(NetBSD) || !#system(unix) || !#system(bsd)
#  error
# endif
#elif #system(NetBSD)
# error
#endif

#if defined __OpenBSD__
# if !#system(OpenBSD) || !#system(unix) || !#system(bsd)
#  error
# endif
#elif #system(OpenBSD)
# error
#endif

#if defined __svr4__ || defined __SYSTYPE_SVR4__
# if !#system(svr4) || !#system(unix)
#  error
# endif
#elif #system(svr4)
# error
#endif

#if defined __hpux__
# if !#system(hpux) || !#system(unix)
#  error
# endif
#elif #system(hpux)
# error
#endif

#if defined _AIX
# if !#system(aix) || !#system(unix)
#  error
# endif
#elif #system(aix)
# error
#endif

#if defined __lynx__
# if !#system(lynx) || !#system(unix)
#  error
# endif
#elif #system(lynx)
# error
#endif

#if ( defined __unix__ && !defined __CYGWIN__ ) || defined _AIX \
    || defined __vxworks
# if !#system(unix)
#  error
# endif
#elif #system(unix)
# error
#endif

#if defined __rtems__
# if !#system(rtems)
#  error
# endif
#elif #system(rtems)
# error
#endif

#if defined __vms__
# if !#system(vms)
#  error
# endif
#elif #system(vms)
# error
#endif

#if defined __mvs__
# if !#system(mvs)
#  error
# endif
#elif #system(mvs)
# error
#endif

#if defined __MSDOS__
# if !#system(msdos)
#  error
# endif
#elif #system(msdos)
# error
#endif

#if defined __WINNT__ || defined __CYGWIN__
# if !#system(winnt)
#  error
# endif
#elif #system(winnt)
# error
#endif

#if defined __netware__
# if !#system(netware)
#  error
# endif
#elif #system(netware)
# error
#endif


/* Check for #cpu and #machine assertions.  */

#if defined __arc__
# if !#cpu(arc) || !#machine(arc)
#  error
# endif
#elif #cpu(arc) || #machine(arc)
# error
#endif

#if defined __alpha__
# if !#cpu(alpha) || !#machine(alpha) \
	|| (defined __alpha_cix__ && !#cpu(cix)) \
	|| (!defined __alpha_cix__ && #cpu(cix)) \
	|| (defined __alpha_fix__ && !#cpu(fix)) \
	|| (!defined __alpha_fix__ && #cpu(fix)) \
	|| (defined __alpha_bwx__ && !#cpu(bwx)) \
	|| (!defined __alpha_bwx__ && #cpu(bwx)) \
	|| (defined __alpha_max__ && !#cpu(max)) \
	|| (!defined __alpha_max__ && #cpu(max)) \
	|| (defined __alpha_ev6__ && !#cpu(ev6)) \
	|| (!defined __alpha_ev6__ && #cpu(ev6)) \
	|| (defined __alpha_ev5__ && !#cpu(ev5)) \
	|| (!defined __alpha_ev5__ && #cpu(ev5)) \
	|| (defined __alpha_ev4__ && !#cpu(ev4)) \
	|| (!defined __alpha_ev4__ && #cpu(ev4))
#  error
# endif
#elif #cpu(alpha) || #machine(alpha) || #cpu(cix) || #cpu(fix) || #cpu(bwx) \
	|| #cpu(max) || #cpu(ev6) || #cpu(ev5) || #cpu(ev4)
# error
#endif

#if defined __arm__
# if !#cpu(arm) || !#machine(arm)
#  error
# endif
#elif #cpu(arm) || #machine(arm)
# error
#endif

#if defined __cris__
# if !#cpu(cris) || !#machine(cris)
#  error
# endif
#elif #cpu(cris) || #machine(cris)
# error
#endif

#if defined __fr30__
# if !#cpu(fr30) || !#machine(fr30)
#  error
# endif
#elif #cpu(fr30) || #machine(fr30)
# error
#endif

#if defined __frv__
# if !#cpu(frv) || !#machine(frv)
#  error
# endif
#elif #cpu(frv) || #machine(frv)
# error
#endif

#if defined __H8300__ 
# if !#cpu(h8300) || !#machine(h8300) \
  || (defined __H8300__ && (!#cpu(h8300) || !#machine(h8300))) \
  || (defined __H8300H__ && (!#cpu(h8300h) || !#machine(h8300h))) \
  || (!defined __H8300H__ && (#cpu(h8300h) || #machine(h8300h))) \
  || (defined __H8300S__ && (!#cpu(h8300s) || !#machine(h8300s))) \
  || (!defined __H8300S__ && (#cpu(h8300s) || #machine(h8300s)))
#  error
# endif
#elif #cpu(h8300) || #machine(h8300) || #cpu(h8300h) || #machine(h8300h) || \
  #cpu(h8300s) || #machine(h8300s)
# error
#endif

#if defined __hppa__
# if !#cpu(hppa) || !#machine(hppa)
#  error
# endif
#elif #cpu(hppa) || #machine(hppa)
# error
#endif

#if defined __i370__
# if !#cpu(i370) || !#machine(i370)
#  error
# endif
#elif #cpu(i370) || #machine(i370)
# error
#endif

#if defined __x86_64__
# if !#cpu(x86_64) || !#machine(x86_64)
#  error
# endif
#elif #cpu(x86_64) || #machine(x86_64)
# error
#endif

#if defined __i386__
# if !#cpu(i386) || !#machine(i386)
#  error
# endif
#elif #cpu(i386) || #machine(i386)
# error
#endif

#if defined __ia64__
# if !#cpu(ia64) || !#machine(ia64)
#  error
# endif
#elif #cpu(ia64) || #machine(ia64)
# error
#endif

#if defined __iq2000__
# if !#cpu(iq2000) || !#machine(iq2000)
#  error
# endif
#elif #cpu(iq2000) || #machine(iq2000)
# error
#endif

#if defined __M32R__
# if !#cpu(m32r) || !#machine(m32r)
#  error
# endif
#elif #cpu(m32r) || #machine(m32r)
# error
#endif

#if defined __m68k__
# if !#cpu(m68k) || !#machine(m68k)
#  error
# endif
#elif #cpu(m68k) || #machine(m68k)
# error
#endif

#if defined __mcore__
# if !#cpu(mcore) || !#machine(mcore)
#  error
# endif
#elif #cpu(mcore) || #machine(mcore)
# error
#endif

#if defined __mips__
# if !#cpu(mips) || (defined __sgi__ && !#machine(sgi)) \
  || (!defined __sgi__ && !#machine(mips))
#  error
# endif
#elif #cpu(mips) || #machine(sgi) || #machine(mips)
# error
#endif

#if defined __mn10300__
# if !#cpu(mn10300) || !#machine(mn10300)
#  error
# endif
#elif #cpu(mn10300) || #machine(mn10300)
# error
#endif

#if defined __pdp11__
# if !#cpu(pdp11) || !#machine(pdp11)
#  error
# endif
#elif #cpu(pdp11) || #machine(pdp11)
# error
#endif

#if defined __powerpc__ || defined __PPC__
# if defined __powerpc64__
#  if (#cpu(powerpc) || #machine(powerpc) \
       || !#cpu(powerpc64) || !#machine(powerpc64))
#   error
#  endif
# else
#  if (!#cpu(powerpc) || !#machine(powerpc) \
       || #cpu(powerpc64) || #machine(powerpc64))
#   error
#  endif
# endif
#elif (#cpu(powerpc) || #machine(powerpc) \
       || #cpu(powerpc64) || #machine(powerpc64))
# error
#endif

#if defined __rs6000__
# if !#cpu(rs6000) || !#machine(rs6000)
#  error
# endif
#elif #cpu(rs6000) || #machine(rs6000)
# error
#endif

#if defined __s390__
# if !#cpu(s390) || !#machine(s390)
#  error
# endif
#elif #cpu(s390) || #machine(s390)
# error
#endif

#if defined __sh__
# if !#cpu(sh) || !#machine(sh)
#  error
# endif
#elif #cpu(sh) || #machine(sh)
# error
#endif

#if defined __sparc__
# if (defined __arch64__ \
      && (!#cpu(sparc64) || !#machine(sparc64) || #cpu(sparc) || #machine(sparc)))
  || (!defined __arch64__ \
      && (#cpu(sparc64) || #machine(sparc64) || !#cpu(sparc) || !#machine(sparc)))
#  error
# endif
#elif #cpu(sparc64) || #machine(sparc64) || #cpu(sparc) || #machine(sparc)
# error
#endif

#if defined __xstormy16__
# if !#cpu(xstormy16) || !#machine(xstormy16)
#  error
# endif
#elif #cpu(xstormy16) || #machine(xstormy16)
# error
#endif

#if defined __v850__
# if !#cpu(v850) || !#machine(v850)
#  error
# endif
#elif #cpu(v850) || #machine(v850)
# error
#endif

#if defined __vax__
# if !#cpu(vax) || !#machine(vax)
#  error
# endif
#elif #cpu(vax) || #machine(vax)
# error
#endif

#if defined __XTENSA__
# if !#cpu(xtensa) || !#machine(xtensa)
#  error
# endif
#elif #cpu(xtensa) || #machine(xtensa)
# error
#endif

