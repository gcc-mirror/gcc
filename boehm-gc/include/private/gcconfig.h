/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
 
#ifndef GCCONFIG_H

# define GCCONFIG_H

/* Machine dependent parameters.  Some tuning parameters can be found	*/
/* near the top of gc_private.h.					*/

/* Machine specific parts contributed by various people.  See README file. */

/* First a unified test for Linux: */
# if defined(linux) || defined(__linux__)
#    define LINUX
# endif

/* And one for NetBSD: */
# if defined(__NetBSD__)
#    define NETBSD
# endif

/* Determine the machine type: */
# if defined(sun) && defined(mc68000)
#    define M68K
#    define SUNOS4
#    define mach_type_known
# endif
# if defined(hp9000s300)
#    define M68K
#    define HP
#    define mach_type_known
# endif
# if defined(__OpenBSD__) && defined(m68k)
#    define M68K
#    define OPENBSD
#    define mach_type_known
# endif
# if defined(__OpenBSD__) && defined(__sparc__)
#    define SPARC
#    define OPENBSD
#    define mach_type_known
# endif
# if defined(__NetBSD__) && defined(m68k)
#    define M68K
#    define mach_type_known
# endif
# if defined(__NetBSD__) && defined(__powerpc__)
#    define POWERPC
#    define mach_type_known
# endif
# if defined(__NetBSD__) && defined(__arm32__)
#    define ARM32
#    define mach_type_known
# endif
# if defined(vax)
#    define VAX
#    ifdef ultrix
#	define ULTRIX
#    else
#	define BSD
#    endif
#    define mach_type_known
# endif
# if defined(mips) || defined(__mips)
#    define MIPS
#    if !defined(LINUX)
#      if defined(ultrix) || defined(__ultrix) || defined(__NetBSD__)
#	 define ULTRIX
#      else
#	 if defined(_SYSTYPE_SVR4) || defined(SYSTYPE_SVR4) \
	    || defined(__SYSTYPE_SVR4__)
#	   define IRIX5   /* or IRIX 6.X */
#	 else
#	   define RISCOS  /* or IRIX 4.X */
#	 endif
#      endif
#    endif /* !LINUX */
#    if defined(__NetBSD__) && defined(__MIPSEL__)
#      undef ULTRIX
#    endif
#    define mach_type_known
# endif
# if defined(sequent) && (defined(i386) || defined(__i386__))
#    define I386
#    define SEQUENT
#    define mach_type_known
# endif
# if defined(sun) && (defined(i386) || defined(__i386__))
#    define I386
#    define SUNOS5
#    define mach_type_known
# endif
# if (defined(__OS2__) || defined(__EMX__)) && defined(__32BIT__)
#    define I386
#    define OS2
#    define mach_type_known
# endif
# if defined(ibm032)
#   define RT
#   define mach_type_known
# endif
# if defined(sun) && (defined(sparc) || defined(__sparc))
#   define SPARC
    /* Test for SunOS 5.x */
#     include <errno.h>
#     ifdef ECHRNG
#       define SUNOS5
#     else
#	define SUNOS4
#     endif
#   define mach_type_known
# endif
# if defined(sparc) && defined(unix) && !defined(sun) && !defined(linux) \
     && !defined(__OpenBSD__) && !(__NetBSD__)
#   define SPARC
#   define DRSNX
#   define mach_type_known
# endif
# if defined(_IBMR2)
#   define RS6000
#   define mach_type_known
# endif
# if defined(__NetBSD__) && defined(__sparc__)
#   define SPARC
#   define mach_type_known
# endif
# if defined(_M_XENIX) && defined(_M_SYSV) && defined(_M_I386)
	/* The above test may need refinement	*/
#   define I386
#   if defined(_SCO_ELF)
#     define SCO_ELF
#   else
#     define SCO
#   endif
#   define mach_type_known
# endif
# if defined(_AUX_SOURCE)
#   define M68K
#   define SYSV
#   define mach_type_known
# endif
# if defined(_PA_RISC1_0) || defined(_PA_RISC1_1) || defined(_PA_RISC2_0) \
     || defined(hppa) || defined(__hppa__)
#   define HP_PA
#   ifndef LINUX
#     define HPUX
#   endif
#   define mach_type_known
# endif
# if defined(__BEOS__) && defined(_X86_)
#    define I386
#    define BEOS
#    define mach_type_known
# endif
# if defined(LINUX) && (defined(i386) || defined(__i386__))
#    define I386
#    define mach_type_known
# endif
# if defined(LINUX) && (defined(__ia64__) || defined(__ia64))
#    define IA64
#    define mach_type_known
# endif
# if defined(LINUX) && defined(powerpc)
#    define POWERPC
#    define mach_type_known
# endif
# if defined(LINUX) && defined(__mc68000__)
#    define M68K
#    define mach_type_known
# endif
# if defined(LINUX) && (defined(sparc) || defined(__sparc__))
#    define SPARC
#    define mach_type_known
# endif
# if defined(LINUX) && defined(__arm__)
#    define ARM32
#    define mach_type_known
# endif
# if defined(LINUX) && defined(__sh__)
#    define SH
#    define mach_type_known
# endif
# if defined(__alpha) || defined(__alpha__)
#   define ALPHA
#   if !defined(LINUX) && !defined(NETBSD)
#     define OSF1	/* a.k.a Digital Unix */
#   endif
#   define mach_type_known
# endif
# if defined(_AMIGA) && !defined(AMIGA)
#   define AMIGA
# endif
# ifdef AMIGA 
#   define M68K
#   define mach_type_known
# endif
# if defined(THINK_C) || defined(__MWERKS__) && !defined(__powerc)
#   define M68K
#   define MACOS
#   define mach_type_known
# endif
# if defined(__MWERKS__) && defined(__powerc)
#   define POWERPC
#   define MACOS
#   define mach_type_known
# endif
# if defined(macosx) || \
     defined(__APPLE__) && defined(__MACH__) && defined(__ppc__)
#    define MACOSX
#    define POWERPC
#    define mach_type_known
# endif
# if defined(__APPLE__) && defined(__MACH__) && defined(__i386__)
#    define MACOSX
#    define I386
     --> Not really supported, but at least we recognize it.
# endif
# if defined(NeXT) && defined(mc68000)
#   define M68K
#   define NEXT
#   define mach_type_known
# endif
# if defined(NeXT) && (defined(i386) || defined(__i386__))
#   define I386
#   define NEXT
#   define mach_type_known
# endif
# if defined(__OpenBSD__) && (defined(i386) || defined(__i386__))
#   define I386
#   define OPENBSD
#   define mach_type_known
# endif
# if defined(__FreeBSD__) && (defined(i386) || defined(__i386__))
#   define I386
#   define FREEBSD
#   define mach_type_known
# endif
# if defined(__NetBSD__) && (defined(i386) || defined(__i386__))
#   define I386
#   define mach_type_known
# endif
# if defined(bsdi) && (defined(i386) || defined(__i386__))
#    define I386
#    define BSDI
#    define mach_type_known
# endif
# if !defined(mach_type_known) && defined(__386BSD__)
#   define I386
#   define THREE86BSD
#   define mach_type_known
# endif
# if defined(_CX_UX) && defined(_M88K)
#   define M88K
#   define CX_UX
#   define mach_type_known
# endif
# if defined(DGUX)
#   define M88K
    /* DGUX defined */
#   define mach_type_known
# endif
# if defined(_WIN32_WCE)
    /* SH3, SH4, MIPS already defined for corresponding architectures */
#   if defined(SH3) || defined(SH4)
#     define SH
#   endif
#   if defined(x86)
#     define I386
#   endif
#   if defined(ARM)
#     define ARM32
#   endif
#   define MSWINCE
#   define mach_type_known
# else
#   if (defined(_MSDOS) || defined(_MSC_VER)) && (_M_IX86 >= 300) \
        || defined(_WIN32) && !defined(__CYGWIN32__) && !defined(__CYGWIN__)
#     define I386
#     define MSWIN32	/* or Win32s */
#     define mach_type_known
#   endif
# endif
# if defined(__DJGPP__)
#   define I386
#   ifndef DJGPP
#     define DJGPP  /* MSDOS running the DJGPP port of GCC */
#   endif
#   define mach_type_known
# endif
# if defined(__CYGWIN32__) || defined(__CYGWIN__)
#   define I386
#   define CYGWIN32
#   define mach_type_known
# endif
# if defined(__MINGW32__)
#   define I386
#   define MSWIN32
#   define mach_type_known
# endif
# if defined(__BORLANDC__)
#   define I386
#   define MSWIN32
#   define mach_type_known
# endif
# if defined(_UTS) && !defined(mach_type_known)
#   define S370
#   define UTS4
#   define mach_type_known
# endif
# if defined(__pj__)
#   define PJ
#   define mach_type_known
# endif
# if defined(__embedded__) && defined(PPC)
#   define POWERPC
#   define NOSYS
#   define mach_type_known
# endif
/* Ivan Demakov */
# if defined(__WATCOMC__) && defined(__386__)
#   define I386
#   if !defined(OS2) && !defined(MSWIN32) && !defined(DOS4GW)
#     if defined(__OS2__)
#       define OS2
#     else
#       if defined(__WINDOWS_386__) || defined(__NT__)
#         define MSWIN32
#       else
#         define DOS4GW
#       endif
#     endif
#   endif
#   define mach_type_known
# endif
# if defined(__s390__) && defined(LINUX)
#    define S370
#    define mach_type_known
# endif

/* Feel free to add more clauses here */

/* Or manually define the machine type here.  A machine type is 	*/
/* characterized by the architecture.  Some				*/
/* machine types are further subdivided by OS.				*/
/* the macros ULTRIX, RISCOS, and BSD to distinguish.			*/
/* Note that SGI IRIX is treated identically to RISCOS.			*/
/* SYSV on an M68K actually means A/UX.					*/
/* The distinction in these cases is usually the stack starting address */
# ifndef mach_type_known
	--> unknown machine type
# endif
		    /* Mapping is: M68K       ==> Motorola 680X0	*/
		    /*		   (SUNOS4,HP,NEXT, and SYSV (A/UX),	*/
		    /*		   MACOS and AMIGA variants)		*/
		    /*             I386       ==> Intel 386	 	*/
		    /*		    (SEQUENT, OS2, SCO, LINUX, NETBSD,	*/
		    /*		     FREEBSD, THREE86BSD, MSWIN32,	*/
		    /* 		     BSDI,SUNOS5, NEXT, other variants)	*/
                    /*             NS32K      ==> Encore Multimax 	*/
                    /*             MIPS       ==> R2000 or R3000	*/
                    /*			(RISCOS, ULTRIX variants)	*/
                    /*		   VAX	      ==> DEC VAX		*/
                    /*			(BSD, ULTRIX variants)		*/
                    /*		   RS6000     ==> IBM RS/6000 AIX3.X	*/
                    /*		   RT	      ==> IBM PC/RT		*/
                    /*		   HP_PA      ==> HP9000/700 & /800	*/
                    /*				  HP/UX, LINUX			*/
		    /*		   SPARC      ==> SPARC	v7/v8/v9	*/
		    /*			(SUNOS4, SUNOS5, LINUX,		*/
		    /*			 DRSNX variants)		*/
		    /* 		   ALPHA      ==> DEC Alpha 		*/
		    /*			(OSF1 and LINUX variants)	*/
		    /* 		   M88K       ==> Motorola 88XX0        */
		    /* 		        (CX_UX and DGUX)		*/
		    /* 		   S370	      ==> 370-like machine	*/
		    /* 			running Amdahl UTS4		*/
		    /*			or a 390 running LINUX		*/
		    /* 		   ARM32      ==> Intel StrongARM	*/
		    /* 		   IA64	      ==> Intel IA64		*/
		    /*				  (e.g. Itanium)	*/
		    /*		   SH	      ==> Hitachi SuperH	*/
		    /* 			(LINUX & MSWINCE)		*/


/*
 * For each architecture and OS, the following need to be defined:
 *
 * CPP_WORD_SZ is a simple integer constant representing the word size.
 * in bits.  We assume byte addressibility, where a byte has 8 bits.
 * We also assume CPP_WORD_SZ is either 32 or 64.
 * (We care about the length of pointers, not hardware
 * bus widths.  Thus a 64 bit processor with a C compiler that uses
 * 32 bit pointers should use CPP_WORD_SZ of 32, not 64. Default is 32.)
 *
 * MACH_TYPE is a string representation of the machine type.
 * OS_TYPE is analogous for the OS.
 *
 * ALIGNMENT is the largest N, such that
 * all pointer are guaranteed to be aligned on N byte boundaries.
 * defining it to be 1 will always work, but perform poorly.
 *
 * DATASTART is the beginning of the data segment.
 * On UNIX systems, the collector will scan the area between DATASTART
 * and DATAEND for root pointers.
 *
 * DATAEND, if not &end.
 *
 * ALIGN_DOUBLE of GC_malloc should return blocks aligned to twice
 * the pointer size.
 *
 * STACKBOTTOM is the cool end of the stack, which is usually the
 * highest address in the stack.
 * Under PCR or OS/2, we have other ways of finding thread stacks.
 * For each machine, the following should:
 * 1) define STACK_GROWS_UP if the stack grows toward higher addresses, and
 * 2) define exactly one of
 *	STACKBOTTOM (should be defined to be an expression)
 *	HEURISTIC1
 *	HEURISTIC2
 * If either of the last two macros are defined, then STACKBOTTOM is computed
 * during collector startup using one of the following two heuristics:
 * HEURISTIC1:  Take an address inside GC_init's frame, and round it up to
 *		the next multiple of STACK_GRAN.
 * HEURISTIC2:  Take an address inside GC_init's frame, increment it repeatedly
 *		in small steps (decrement if STACK_GROWS_UP), and read the value
 *		at each location.  Remember the value when the first
 *		Segmentation violation or Bus error is signalled.  Round that
 *		to the nearest plausible page boundary, and use that instead
 *		of STACKBOTTOM.
 *
 * Gustavo Rodriguez-Rivera points out that on most (all?) Unix machines,
 * the value of environ is a pointer that can serve as STACKBOTTOM.
 * I expect that HEURISTIC2 can be replaced by this approach, which
 * interferes far less with debugging.  However it has the disadvantage
 * that it's confused by a putenv call before the collector is initialized.
 * This could be dealt with by intercepting putenv ...
 *
 * If no expression for STACKBOTTOM can be found, and neither of the above
 * heuristics are usable, the collector can still be used with all of the above
 * undefined, provided one of the following is done:
 * 1) GC_mark_roots can be changed to somehow mark from the correct stack(s)
 *    without reference to STACKBOTTOM.  This is appropriate for use in
 *    conjunction with thread packages, since there will be multiple stacks.
 *    (Allocating thread stacks in the heap, and treating them as ordinary
 *    heap data objects is also possible as a last resort.  However, this is
 *    likely to introduce significant amounts of excess storage retention
 *    unless the dead parts of the thread stacks are periodically cleared.)
 * 2) Client code may set GC_stackbottom before calling any GC_ routines.
 *    If the author of the client code controls the main program, this is
 *    easily accomplished by introducing a new main program, setting
 *    GC_stackbottom to the address of a local variable, and then calling
 *    the original main program.  The new main program would read something
 *    like:
 *
 *		# include "gc_private.h"
 *
 *		main(argc, argv, envp)
 *		int argc;
 *		char **argv, **envp;
 *		{
 *		    int dummy;
 *
 *		    GC_stackbottom = (ptr_t)(&dummy);
 *		    return(real_main(argc, argv, envp));
 *		}
 *
 *
 * Each architecture may also define the style of virtual dirty bit
 * implementation to be used:
 *   MPROTECT_VDB: Write protect the heap and catch faults.
 *   PROC_VDB: Use the SVR4 /proc primitives to read dirty bits.
 *
 * An architecture may define DYNAMIC_LOADING if dynamic_load.c
 * defined GC_register_dynamic_libraries() for the architecture.
 *
 * An architecture may define PREFETCH(x) to preload the cache with *x.
 * This defaults to a no-op.
 *
 * PREFETCH_FOR_WRITE(x) is used if *x is about to be written.
 *
 * An architecture may also define CLEAR_DOUBLE(x) to be a fast way to
 * clear the two words at GC_malloc-aligned address x.  By default,
 * word stores of 0 are used instead.
 */


# define STACK_GRAN 0x1000000
# ifdef M68K
#   define MACH_TYPE "M68K"
#   define ALIGNMENT 2
#   ifdef OPENBSD
#	define OS_TYPE "OPENBSD"
#	define HEURISTIC2
	extern char etext;
#	define DATASTART ((ptr_t)(&etext))
#   endif
#   ifdef NETBSD
#	define OS_TYPE "NETBSD"
#	define HEURISTIC2
	extern char etext;
#	define DATASTART ((ptr_t)(&etext))
#   endif
#   ifdef LINUX
#       define OS_TYPE "LINUX"
#       define STACKBOTTOM ((ptr_t)0xf0000000)
#       define MPROTECT_VDB
#       ifdef __ELF__
#            define DYNAMIC_LOADING
#	     include <features.h>
#	     if defined(__GLIBC__)&& __GLIBC__>=2
#              define LINUX_DATA_START
#	     else /* !GLIBC2 */
               extern char **__environ;
#              define DATASTART ((ptr_t)(&__environ))
                             /* hideous kludge: __environ is the first */
                             /* word in crt0.o, and delimits the start */
                             /* of the data segment, no matter which   */
                             /* ld options were passed through.        */
                             /* We could use _etext instead, but that  */
                             /* would include .rodata, which may       */
                             /* contain large read-only data tables    */
                             /* that we'd rather not scan.             */
#	     endif /* !GLIBC2 */
             extern int _end;
#            define DATAEND (&_end)
#       else
             extern int etext;
#            define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#       endif
#   endif
#   ifdef SUNOS4
#	define OS_TYPE "SUNOS4"
	extern char etext;
#	define DATASTART ((ptr_t)((((word) (&etext)) + 0x1ffff) & ~0x1ffff))
#	define HEURISTIC1	/* differs	*/
#	define DYNAMIC_LOADING
#   endif
#   ifdef HP
#	define OS_TYPE "HP"
	extern char etext;
#       define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#       define STACKBOTTOM ((ptr_t) 0xffeffffc)
			      /* empirically determined.  seems to work. */
#  	include <unistd.h>
#	define GETPAGESIZE() sysconf(_SC_PAGE_SIZE)
#   endif
#   ifdef SYSV
#	define OS_TYPE "SYSV"
	extern etext;
#   	define DATASTART ((ptr_t)((((word) (&etext)) + 0x3fffff) \
				   & ~0x3fffff) \
				  +((word)&etext & 0x1fff))
	/* This only works for shared-text binaries with magic number 0413.
	   The other sorts of SysV binaries put the data at the end of the text,
	   in which case the default of &etext would work.  Unfortunately,
	   handling both would require having the magic-number available.
	   	   		-- Parag
	   */
#	define STACKBOTTOM ((ptr_t)0xFFFFFFFE)
			/* The stack starts at the top of memory, but   */
			/* 0x0 cannot be used as setjump_test complains */
			/* that the stack direction is incorrect.  Two  */
			/* bytes down from 0x0 should be safe enough.   */
			/* 		--Parag				*/
#   	include <sys/mmu.h>
#	define GETPAGESIZE() PAGESIZE	/* Is this still right? */
#   endif
#   ifdef AMIGA
#	define OS_TYPE "AMIGA"
 	    	/* STACKBOTTOM and DATASTART handled specially	*/
 	    	/* in os_dep.c					*/
# 	define DATAEND	/* not needed */
#	define GETPAGESIZE() 4096
#   endif
#   ifdef MACOS
#     ifndef __LOWMEM__
#     include <LowMem.h>
#     endif
#     define OS_TYPE "MACOS"
			/* see os_dep.c for details of global data segments. */
#     define STACKBOTTOM ((ptr_t) LMGetCurStackBase())
#     define DATAEND	/* not needed */
#     define GETPAGESIZE() 4096
#   endif
#   ifdef NEXT
#	define OS_TYPE "NEXT"
#	define DATASTART ((ptr_t) get_etext())
#	define STACKBOTTOM ((ptr_t) 0x4000000)
#	define DATAEND	/* not needed */
#   endif
# endif

# ifdef POWERPC
#   define MACH_TYPE "POWERPC"
#   ifdef MACOS
#     define ALIGNMENT 2  /* Still necessary?  Could it be 4?	*/
#     ifndef __LOWMEM__
#     include <LowMem.h>
#     endif
#     define OS_TYPE "MACOS"
			/* see os_dep.c for details of global data segments. */
#     define STACKBOTTOM ((ptr_t) LMGetCurStackBase())
#     define DATAEND  /* not needed */
#   endif
#   ifdef LINUX
#     define ALIGNMENT 4	/* Guess.  Can someone verify?	*/
				/* This was 2, but that didn't sound right. */
#     define OS_TYPE "LINUX"
#     define HEURISTIC1
#     define DYNAMIC_LOADING
#     undef STACK_GRAN
#     define STACK_GRAN 0x10000000
	/* Stack usually starts at 0x80000000 */
#     define LINUX_DATA_START
      extern int _end;
#     define DATAEND (&_end)
#   endif
#   ifdef MACOSX
      /* There are reasons to suspect this may not be reliable. 	*/
#     define ALIGNMENT 4
#     define OS_TYPE "MACOSX"
#     define DATASTART ((ptr_t) get_etext())
#     define STACKBOTTOM ((ptr_t) 0xc0000000)
#     define DATAEND	/* not needed */
#     define MPROTECT_VDB
#     include <unistd.h>
#     define GETPAGESIZE() getpagesize()
#   endif
#   ifdef NETBSD
#     define ALIGNMENT 4
#     define OS_TYPE "NETBSD"
#     define HEURISTIC2
      extern char etext;
#     define DATASTART GC_data_start
#     define DYNAMIC_LOADING
#   endif
#   ifdef NOSYS
#     define ALIGNMENT 4
#     define OS_TYPE "NOSYS"
      extern void __end, __dso_handle;
#     define DATASTART (&__dso_handle)  /* OK, that's ugly.  */
#     define DATAEND (&__end)
	/* Stack starts at 0xE0000000 for the simulator.  */
#     undef STACK_GRAN
#     define STACK_GRAN 0x10000000
#     define HEURISTIC1
#   endif
# endif

# ifdef VAX
#   define MACH_TYPE "VAX"
#   define ALIGNMENT 4	/* Pointers are longword aligned by 4.2 C compiler */
    extern char etext;
#   define DATASTART ((ptr_t)(&etext))
#   ifdef BSD
#	define OS_TYPE "BSD"
#	define HEURISTIC1
			/* HEURISTIC2 may be OK, but it's hard to test. */
#   endif
#   ifdef ULTRIX
#	define OS_TYPE "ULTRIX"
#	define STACKBOTTOM ((ptr_t) 0x7fffc800)
#   endif
# endif

# ifdef RT
#   define MACH_TYPE "RT"
#   define ALIGNMENT 4
#   define DATASTART ((ptr_t) 0x10000000)
#   define STACKBOTTOM ((ptr_t) 0x1fffd800)
# endif

# ifdef SPARC
#   define MACH_TYPE "SPARC"
#   if defined(__arch64__) || defined(__sparcv9)
#     define ALIGNMENT 8
#   else
#     define ALIGNMENT 4	/* Required by hardware	*/
#   endif
#   define ALIGN_DOUBLE
#   ifdef SUNOS5
#	define OS_TYPE "SUNOS5"
	extern int _etext;
	extern int _end;
	extern char * GC_SysVGetDataStart();
#       define DATASTART (ptr_t)GC_SysVGetDataStart(0x10000, &_etext)
#	define DATAEND (&_end)
#	ifndef USE_MMAP
#	    define USE_MMAP
#	endif
#       ifdef USE_MMAP
#         define HEAP_START (ptr_t)0x40000000
#       else
#	  define HEAP_START DATAEND
#       endif
#	define PROC_VDB
/*	HEURISTIC1 reportedly no longer works under 2.7.  		*/
/*  	HEURISTIC2 probably works, but this appears to be preferable.	*/
/*	Apparently USRSTACK is defined to be USERLIMIT, but in some	*/
/* 	installations that's undefined.  We work around this with a	*/
/*	gross hack:							*/
#       include <sys/vmparam.h>
#	ifdef USERLIMIT
	  /* This should work everywhere, but doesn't.	*/
#	  define STACKBOTTOM USRSTACK
#       else
#	  define HEURISTIC2
#       endif
#	include <unistd.h>
#       define GETPAGESIZE()  sysconf(_SC_PAGESIZE)
		/* getpagesize() appeared to be missing from at least one */
		/* Solaris 5.4 installation.  Weird.			  */
#	define DYNAMIC_LOADING
#   endif
#   ifdef SUNOS4
#	define OS_TYPE "SUNOS4"
	/* [If you have a weak stomach, don't read this.]		*/
	/* We would like to use:					*/
/* #       define DATASTART ((ptr_t)((((word) (&etext)) + 0x1fff) & ~0x1fff)) */
	/* This fails occasionally, due to an ancient, but very 	*/
	/* persistent ld bug.  &etext is set 32 bytes too high.		*/
	/* We instead read the text segment size from the a.out		*/
	/* header, which happens to be mapped into our address space	*/
	/* at the start of the text segment.  The detective work here	*/
	/* was done by Robert Ehrlich, Manuel Serrano, and Bernard	*/
	/* Serpette of INRIA.						*/
	/* This assumes ZMAGIC, i.e. demand-loadable executables.	*/
#	define TEXTSTART 0x2000
#       define DATASTART ((ptr_t)(*(int *)(TEXTSTART+0x4)+TEXTSTART))
#	define MPROTECT_VDB
#	define HEURISTIC1
# 	define DYNAMIC_LOADING
#   endif
#   ifdef DRSNX
#       define CPP_WORDSZ 32
#	define OS_TYPE "DRSNX"
	extern char * GC_SysVGetDataStart();
	extern int etext;
#       define DATASTART (ptr_t)GC_SysVGetDataStart(0x10000, &etext)
#	define MPROTECT_VDB
#       define STACKBOTTOM ((ptr_t) 0xdfff0000)
#	define DYNAMIC_LOADING
#   endif
#   ifdef LINUX
#     define OS_TYPE "LINUX"
#     ifdef __ELF__
#       define DYNAMIC_LOADING
#     else
          Linux Sparc/a.out not supported
#     endif
      extern int _end;
      extern int _etext;
#     define DATAEND (&_end)
#     define SVR4
#     ifdef __arch64__
#       define STACKBOTTOM ((ptr_t) 0x80000000000ULL)
#	define DATASTART (ptr_t)GC_SysVGetDataStart(0x100000, &_etext)
#	define CPP_WORDSZ 64
#     else
#       define STACKBOTTOM ((ptr_t) 0xf0000000)
#	define DATASTART (ptr_t)GC_SysVGetDataStart(0x10000, &_etext)
#     endif
#   endif
#   ifdef OPENBSD
#     define OS_TYPE "OPENBSD"
#     define STACKBOTTOM ((ptr_t) 0xf8000000)
      extern int etext;
#     define DATASTART ((ptr_t)(&etext))
#   endif
#   ifdef NETBSD
#     define OS_TYPE "NETBSD"
#     define HEURISTIC2
#     ifdef __ELF__
#	define DATASTART GC_data_start
#	define DYNAMIC_LOADING
#     else
	extern char etext;
#	define DATASTART ((ptr_t)(&etext))
#     endif
#   endif
# endif

# ifdef I386
#   define MACH_TYPE "I386"
#   define ALIGNMENT 4	/* Appears to hold for all "32 bit" compilers	*/
			/* except Borland.  The -a4 option fixes 	*/
			/* Borland.					*/
                        /* Ivan Demakov: For Watcom the option is -zp4. */
#   ifndef SMALL_CONFIG
#     define ALIGN_DOUBLE /* Not strictly necessary, but may give speed   */
			  /* improvement on Pentiums.			  */
#   endif
#   ifdef SEQUENT
#	define OS_TYPE "SEQUENT"
	extern int etext;
#       define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#       define STACKBOTTOM ((ptr_t) 0x3ffff000) 
#   endif
#   ifdef BEOS
#     define OS_TYPE "BEOS"
#     include <OS.h>
#     define GETPAGESIZE() B_PAGE_SIZE
      extern int etext;
#     define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#   endif
#   ifdef SUNOS5
#	define OS_TYPE "SUNOS5"
  	extern int etext, _start;
  	extern char * GC_SysVGetDataStart();
#       define DATASTART GC_SysVGetDataStart(0x1000, &etext)
/*	# define STACKBOTTOM ((ptr_t)(&_start)) worked through 2.7,  	*/
/*      but reportedly breaks under 2.8.  It appears that the stack	*/
/* 	base is a property of the executable, so this should not break	*/
/* 	old executables.						*/
/*  	HEURISTIC2 probably works, but this appears to be preferable.	*/
#       include <sys/vmparam.h>
#	define STACKBOTTOM USRSTACK
/** At least in Solaris 2.5, PROC_VDB gives wrong values for dirty bits. */
/*#	define PROC_VDB*/
#	define DYNAMIC_LOADING
#	ifndef USE_MMAP
#	    define USE_MMAP
#	endif
#       ifdef USE_MMAP
#         define HEAP_START (ptr_t)0x40000000
#       else
#	  define HEAP_START DATAEND
#       endif
#   endif
#   ifdef SCO
#	define OS_TYPE "SCO"
	extern int etext;
#   	define DATASTART ((ptr_t)((((word) (&etext)) + 0x3fffff) \
				  & ~0x3fffff) \
				 +((word)&etext & 0xfff))
#	define STACKBOTTOM ((ptr_t) 0x7ffffffc)
#   endif
#   ifdef SCO_ELF
#       define OS_TYPE "SCO_ELF"
        extern int etext;
#       define DATASTART ((ptr_t)(&etext))
#       define STACKBOTTOM ((ptr_t) 0x08048000)
#       define DYNAMIC_LOADING
#	define ELF_CLASS ELFCLASS32
#   endif
#   ifdef LINUX
#	define OS_TYPE "LINUX"
#       define LINUX_STACKBOTTOM
#	if 0
#	  define HEURISTIC1
#         undef STACK_GRAN
#         define STACK_GRAN 0x10000000
	  /* STACKBOTTOM is usually 0xc0000000, but this changes with	*/
	  /* different kernel configurations.  In particular, systems	*/
	  /* with 2GB physical memory will usually move the user	*/
	  /* address space limit, and hence initial SP to 0x80000000.	*/
#       endif
#       if !defined(LINUX_THREADS) || !defined(REDIRECT_MALLOC)
#	    define MPROTECT_VDB
#	else
	    /* We seem to get random errors in incremental mode,	*/
	    /* possibly because Linux threads is itself a malloc client */
	    /* and can't deal with the signals.				*/
#	endif
#       ifdef __ELF__
#            define DYNAMIC_LOADING
#	     ifdef UNDEFINED	/* includes ro data */
	       extern int _etext;
#              define DATASTART ((ptr_t)((((word) (&_etext)) + 0xfff) & ~0xfff))
#	     endif
#	     include <features.h>
#	     if defined(__GLIBC__) && __GLIBC__ >= 2
#		 define LINUX_DATA_START
#	     else
     	         extern char **__environ;
#                define DATASTART ((ptr_t)(&__environ))
			      /* hideous kludge: __environ is the first */
			      /* word in crt0.o, and delimits the start */
			      /* of the data segment, no matter which   */
			      /* ld options were passed through.        */
			      /* We could use _etext instead, but that  */
			      /* would include .rodata, which may       */
			      /* contain large read-only data tables    */
			      /* that we'd rather not scan.		*/
#	     endif
	     extern int _end;
#	     define DATAEND (&_end)
#	else
	     extern int etext;
#            define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#       endif
#	ifdef USE_I686_PREFETCH
#	  define PREFETCH(x) \
	    __asm__ __volatile__ ("	prefetchnta	%0": : "m"(*(char *)(x)))
	    /* Empirically prefetcht0 is much more effective at reducing	*/
	    /* cache miss stalls for the targetted load instructions.  But it	*/
	    /* seems to interfere enough with other cache traffic that the net	*/
	    /* result is worse than prefetchnta.				*/
#         if 0 
	    /* Using prefetches for write seems to have a slight negative	*/
	    /* impact on performance, at least for a PIII/500.			*/
#	    define PREFETCH_FOR_WRITE(x) \
	      __asm__ __volatile__ ("	prefetcht0	%0": : "m"(*(char *)(x)))
#	  endif
#	endif
#	ifdef USE_3DNOW_PREFETCH
#	  define PREFETCH(x) \
	    __asm__ __volatile__ ("	prefetch	%0": : "m"(*(char *)(x)))
#	  define PREFETCH_FOR_WRITE(x) \
	    __asm__ __volatile__ ("	prefetchw	%0": : "m"(*(char *)(x)))
#	endif
#   endif
#   ifdef CYGWIN32
#       define OS_TYPE "CYGWIN32"
          extern int _data_start__;
          extern int _data_end__;
          extern int _bss_start__;
          extern int _bss_end__;
  	/* For binutils 2.9.1, we have			*/
  	/*	DATASTART   = _data_start__		*/
  	/*	DATAEND	    = _bss_end__		*/
  	/* whereas for some earlier versions it was	*/
  	/*	DATASTART   = _bss_start__		*/
  	/*	DATAEND	    = _data_end__		*/
  	/* To get it right for both, we take the	*/
  	/* minumum/maximum of the two.			*/
#   	define MAX(x,y) ((x) > (y) ? (x) : (y))
#   	define MIN(x,y) ((x) < (y) ? (x) : (y))
#       define DATASTART ((ptr_t) MIN(&_data_start__, &_bss_start__))
#       define DATAEND	 ((ptr_t) MAX(&_data_end__, &_bss_end__))
#	undef STACK_GRAN
#       define STACK_GRAN 0x10000
#       define HEURISTIC1
#   endif
#   ifdef OS2
#	define OS_TYPE "OS2"
 	    	/* STACKBOTTOM and DATASTART are handled specially in 	*/
		/* os_dep.c. OS2 actually has the right			*/
		/* system call!						*/
#	define DATAEND	/* not needed */
#	define USE_GENERIC_PUSH_REGS
#   endif
#   ifdef MSWIN32
#	define OS_TYPE "MSWIN32"
		/* STACKBOTTOM and DATASTART are handled specially in 	*/
		/* os_dep.c.						*/
#       ifndef __WATCOMC__
#	  define MPROTECT_VDB
#	endif
#       define DATAEND  /* not needed */
#   endif
#   ifdef MSWINCE
#	define OS_TYPE "MSWINCE"
#       define DATAEND  /* not needed */
#   endif
#   ifdef DJGPP
#       define OS_TYPE "DJGPP"
#       include "stubinfo.h"
        extern int etext;
        extern int _stklen;
        extern int __djgpp_stack_limit;
#       define DATASTART ((ptr_t)((((word) (&etext)) + 0x1ff) & ~0x1ff))
/* #       define STACKBOTTOM ((ptr_t)((word) _stubinfo + _stubinfo->size \
                                                     + _stklen)) */
#       define STACKBOTTOM ((ptr_t)((word) __djgpp_stack_limit + _stklen))
		/* This may not be right.  */
#   endif
#   ifdef OPENBSD
#	define OS_TYPE "OPENBSD"
#   endif
#   ifdef FREEBSD
#	define OS_TYPE "FREEBSD"
#	define MPROTECT_VDB
#	define FREEBSD_STACKBOTTOM
#   endif
#   ifdef NETBSD
#	define OS_TYPE "NETBSD"
#   endif
#   ifdef THREE86BSD
#	define OS_TYPE "THREE86BSD"
#   endif
#   ifdef BSDI
#	define OS_TYPE "BSDI"
#   endif
#   if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) \
        || defined(THREE86BSD) || defined(BSDI)
#	define HEURISTIC2
	extern char etext;
#	define DATASTART ((ptr_t)(&etext))
#   endif
#   ifdef NEXT
#	define OS_TYPE "NEXT"
#	define DATASTART ((ptr_t) get_etext())
#	define STACKBOTTOM ((ptr_t)0xc0000000)
#	define DATAEND	/* not needed */
#   endif
#   ifdef DOS4GW
#     define OS_TYPE "DOS4GW"
      extern long __nullarea;
      extern char _end;
      extern char *_STACKTOP;
      /* Depending on calling conventions Watcom C either precedes
         or does not precedes with undescore names of C-variables.
         Make sure startup code variables always have the same names.  */
      #pragma aux __nullarea "*";
      #pragma aux _end "*";
#     define STACKBOTTOM ((ptr_t) _STACKTOP)
                         /* confused? me too. */
#     define DATASTART ((ptr_t) &__nullarea)
#     define DATAEND ((ptr_t) &_end)
#   endif
# endif

# ifdef NS32K
#   define MACH_TYPE "NS32K"
#   define ALIGNMENT 4
    extern char **environ;
#   define DATASTART ((ptr_t)(&environ))
			      /* hideous kludge: environ is the first   */
			      /* word in crt0.o, and delimits the start */
			      /* of the data segment, no matter which   */
			      /* ld options were passed through.        */
#   define STACKBOTTOM ((ptr_t) 0xfffff000) /* for Encore */
# endif

# ifdef MIPS
#   define MACH_TYPE "MIPS"
/* #   define STACKBOTTOM ((ptr_t)0x7fff8000)  sometimes also works.  */
#   ifdef LINUX
      /* This was developed for a linuxce style platform.  Probably	*/
      /* needs to be tweaked for workstation class machines.		*/
#     define OS_TYPE "LINUX"
      extern int __data_start;
#     define DATASTART ((ptr_t)(&__data_start))
#     define ALIGNMENT 4
#     define USE_GENERIC_PUSH_REGS 1
#     define STACKBOTTOM 0x80000000
	/* In many cases, this should probably use LINUX_STACKBOTTOM 	*/
	/* instead. But some kernel versions seem to give the wrong	*/
	/* value from /proc.						*/
#   endif /* Linux */
#   ifdef ULTRIX
#	define HEURISTIC2
#       define DATASTART (ptr_t)0x10000000
			      /* Could probably be slightly higher since */
			      /* startup code allocates lots of stuff.   */
#	define OS_TYPE "ULTRIX"
#       define ALIGNMENT 4
#   endif
#   ifdef RISCOS
#	define HEURISTIC2
#       define DATASTART (ptr_t)0x10000000
#	define OS_TYPE "RISCOS"
#   	define ALIGNMENT 4  /* Required by hardware */
#   endif
#   ifdef IRIX5
#	define HEURISTIC2
        extern int _fdata;
#       define DATASTART ((ptr_t)(&_fdata))
#       ifdef USE_MMAP
#         define HEAP_START (ptr_t)0x30000000
#       else
#	  define HEAP_START DATASTART
#       endif
			      /* Lowest plausible heap address.		*/
			      /* In the MMAP case, we map there.	*/
			      /* In either case it is used to identify	*/
			      /* heap sections so they're not 		*/
			      /* considered as roots.			*/
#	define OS_TYPE "IRIX5"
#       define MPROTECT_VDB
#       ifdef _MIPS_SZPTR
#	  define CPP_WORDSZ _MIPS_SZPTR
#	  define ALIGNMENT (_MIPS_SZPTR/8)
#	  if CPP_WORDSZ != 64
#	    define ALIGN_DOUBLE
#	  endif
#	else
#         define ALIGNMENT 4
#	  define ALIGN_DOUBLE
#	endif
#	define DYNAMIC_LOADING
#   endif
#   ifdef MSWINCE
#       define OS_TYPE "MSWINCE"
#       define ALIGNMENT 4
#       define DATAEND /* not needed */
#   endif
#   if defined(NETBSD)
      /* This also checked for __MIPSEL__ .  Why?  NETBSD recognition	*/
      /* should be handled at the top of the file.			*/
#     define ALIGNMENT 4
#     define OS_TYPE "NETBSD"
#     define HEURISTIC2
#     define USE_GENERIC_PUSH_REGS 1
#     ifdef __ELF__
        extern int etext;
#       define DATASTART GC_data_start
#       define NEED_FIND_LIMIT
#       define DYNAMIC_LOADING
#     else
#       define DATASTART ((ptr_t) 0x10000000)
#       define STACKBOTTOM ((ptr_t) 0x7ffff000)
#     endif /* _ELF_ */
#  endif
# endif

# ifdef RS6000
#   define MACH_TYPE "RS6000"
#   define ALIGNMENT 4
#   define DATASTART ((ptr_t)0x20000000)
    extern int errno;
#   define STACKBOTTOM ((ptr_t)((ulong)&errno))
#   define DYNAMIC_LOADING
	/* For really old versions of AIX, this may have to be removed. */
# endif

# ifdef HP_PA
#   define MACH_TYPE "HP_PA"
#   define OS_TYPE "HPUX"
#   ifdef __LP64__
#     define CPP_WORDSZ 64
#     define ALIGNMENT 8
#   else
#     define CPP_WORDSZ 32
#     define ALIGNMENT 4
#     define ALIGN_DOUBLE
#   endif
#   if !defined(GC_HPUX_THREADS) && !defined(HPUX_THREADS) \
       && !defined(GC_LINUX_THREADS) && !defined(LINUX_THREADS)
#     ifndef LINUX /* For now. */
#       define MPROTECT_VDB
#     endif
#   else
#     define GENERIC_COMPARE_AND_SWAP
	/* No compare-and-swap instruction.  Use pthread mutexes 	*/
	/* when we absolutely have to.					*/
#     ifdef PARALLEL_MARK
#	define USE_MARK_BYTES
		/* Minimize compare-and-swap usage.		*/
#     endif
#   endif
#   define STACK_GROWS_UP
#   ifdef HPUX
      extern int __data_start;
#     define DATASTART ((ptr_t)(&__data_start))
#     if 0
	/* The following appears to work for 7xx systems running HP/UX	*/
	/* 9.xx Furthermore, it might result in much faster		*/
	/* collections than HEURISTIC2, which may involve scanning	*/
	/* segments that directly precede the stack.  It is not the	*/
	/* default, since it may not work on older machine/OS		*/
	/* combinations. (Thanks to Raymond X.T. Nijssen for uncovering	*/
	/* this.)							*/
#       define STACKBOTTOM ((ptr_t) 0x7b033000)  /* from /etc/conf/h/param.h */
#     else
	/* Gustavo Rodriguez-Rivera suggested changing HEURISTIC2	*/
	/* to this.  Note that the GC must be initialized before the	*/
    	/* first putenv call.						*/
	extern char ** environ;
#       define STACKBOTTOM ((ptr_t)environ)
#     endif
#     define DYNAMIC_LOADING
#     include <unistd.h>
#     define GETPAGESIZE() sysconf(_SC_PAGE_SIZE)
#     ifndef __GNUC__
#       define PREFETCH(x)  { \
                              register long addr = (long)(x); \
                              (void) _asm ("LDW", 0, 0, addr, 0); \
                            }
#     endif
#   endif /* HPUX */
#   ifdef LINUX
#     define OS_TYPE "LINUX"
#     define LINUX_STACKBOTTOM
#     define DYNAMIC_LOADING
#     define LINUX_DATA_START
      extern int _end;
#     define DATAEND (&_end)
#   endif /* LINUX */
# endif /* HP_PA */

# ifdef ALPHA
#   define MACH_TYPE "ALPHA"
#   define ALIGNMENT 8
#   ifdef NETBSD
#	define OS_TYPE "NETBSD"
#	define HEURISTIC2
#	define DATASTART GC_data_start
#	define ELFCLASS32 32
#	define ELFCLASS64 64
#	define ELF_CLASS ELFCLASS64
#   	define CPP_WORDSZ 64
#       define DYNAMIC_LOADING
#   endif
#   ifdef OSF1
#	define OS_TYPE "OSF1"
#   	define DATASTART ((ptr_t) 0x140000000)
	extern int _end;
#   	define DATAEND ((ptr_t) &_end)
 	extern char ** environ;
	/* round up from the value of environ to the nearest page boundary */
	/* Probably breaks if putenv is called before collector 	   */
	/* initialization.						   */
#	define STACKBOTTOM ((ptr_t)(((word)(environ) | (getpagesize()-1))+1))
/* #   	define HEURISTIC2 */
	/* Normally HEURISTIC2 is too conervative, since		*/
	/* the text segment immediately follows the stack.		*/
	/* Hence we give an upper pound.				*/
	/* This is currently unused, since we disabled HEURISTIC2	*/
    	extern int __start;
#   	define HEURISTIC2_LIMIT ((ptr_t)((word)(&__start) & ~(getpagesize()-1)))
#   	define CPP_WORDSZ 64
#   	define MPROTECT_VDB
#   	define DYNAMIC_LOADING
#   endif
#   ifdef LINUX
#       define OS_TYPE "LINUX"
#       define CPP_WORDSZ 64
#       define STACKBOTTOM ((ptr_t) 0x120000000)
#       ifdef __ELF__
#	  define SEARCH_FOR_DATA_START
#	  define DATASTART GC_data_start
#         define DYNAMIC_LOADING
#       else
#           define DATASTART ((ptr_t) 0x140000000)
#       endif
	extern int _end;
#	define DATAEND (&_end)
#	define MPROTECT_VDB
		/* Has only been superficially tested.  May not	*/
		/* work on all versions.			*/
#   endif
# endif

# ifdef IA64
#   define MACH_TYPE "IA64"
#   define ALIGN_DOUBLE
	/* Requires 16 byte alignment for malloc */
#   define ALIGNMENT 8
#   define USE_GENERIC_PUSH_REGS
	/* We need to get preserved registers in addition to register   */
	/* windows.   That's easiest to do with setjmp.			*/
#   ifdef PARALLEL_MARK
#	define USE_MARK_BYTES
	    /* Compare-and-exchange is too expensive to use for 	*/
	    /* setting mark bits.					*/
#   endif
#   ifdef HPUX
	--> needs work
#   endif
#   ifdef LINUX
#       define OS_TYPE "LINUX"
#       define CPP_WORDSZ 64
	/* The following works on NUE and older kernels:	*/
/* #       define STACKBOTTOM ((ptr_t) 0xa000000000000000l)	*/
	/* This does not work on NUE:				*/
#       define LINUX_STACKBOTTOM
	/* We also need the base address of the register stack	*/
	/* backing store.  This is computed in 			*/
	/* GC_linux_register_stack_base based on the following	*/
	/* constants:						*/
#       define BACKING_STORE_ALIGNMENT 0x100000
#       define BACKING_STORE_DISPLACEMENT 0x80000000
	extern char * GC_register_stackbottom;
#	define BACKING_STORE_BASE ((ptr_t)GC_register_stackbottom)
#	define SEARCH_FOR_DATA_START
#	define DATASTART GC_data_start
#       define DYNAMIC_LOADING
#	define MPROTECT_VDB
		/* Requires Linux 2.3.47 or later.	*/
	extern int _end;
#	define DATAEND (&_end)
#       ifdef __GNUC__
#	  define PREFETCH(x) \
	    __asm__ ("	lfetch	[%0]": : "r"((void *)(x)))
#	  define PREFETCH_FOR_WRITE(x) \
	    __asm__ ("	lfetch.excl	[%0]": : "r"((void *)(x)))
#	  define CLEAR_DOUBLE(x) \
	    __asm__ ("	stf.spill	[%0]=f0": : "r"((void *)(x)))
#       endif
#   endif
# endif

# ifdef M88K
#   define MACH_TYPE "M88K"
#   define ALIGNMENT 4
#   define ALIGN_DOUBLE
    extern int etext;
#   ifdef CX_UX
#	define OS_TYPE "CX_UX"
#       define DATASTART ((((word)&etext + 0x3fffff) & ~0x3fffff) + 0x10000)
#   endif
#   ifdef  DGUX
#	define OS_TYPE "DGUX"
	extern char * GC_SysVGetDataStart();
#       define DATASTART (ptr_t)GC_SysVGetDataStart(0x10000, &etext)
#   endif
#   define STACKBOTTOM ((char*)0xf0000000) /* determined empirically */
# endif

# ifdef S370
#   define MACH_TYPE "S370"
#   define ALIGNMENT 4	/* Required by hardware	*/
#   define USE_GENERIC_PUSH_REGS
#   ifdef UTS4
#       define OS_TYPE "UTS4"
        extern int etext;
	extern int _etext;
	extern int _end;
	extern char * GC_SysVGetDataStart();
#       define DATASTART (ptr_t)GC_SysVGetDataStart(0x10000, &_etext)
#	define DATAEND (&_end)
#	define HEURISTIC2
#   endif
#   ifdef LINUX
#       define OS_TYPE "LINUX"
#       define HEURISTIC1
#       define DYNAMIC_LOADING
        extern int __data_start;
#       define DATASTART ((ptr_t)(&__data_start))
#   endif
# endif

# if defined(PJ)
#   define ALIGNMENT 4
    extern int _etext;
#   define DATASTART ((ptr_t)(&_etext))
#   define HEURISTIC1
# endif

# ifdef ARM32
#   define CPP_WORDSZ 32
#   define MACH_TYPE "ARM32"
#   define ALIGNMENT 4
#   ifdef NETBSD
#       define OS_TYPE "NETBSD"
#       define HEURISTIC2
        extern char etext;
#       define DATASTART ((ptr_t)(&etext))
#       define USE_GENERIC_PUSH_REGS
#   endif
#   ifdef LINUX
#       define OS_TYPE "LINUX"
#       define HEURISTIC1
#       undef STACK_GRAN
#       define STACK_GRAN 0x10000000
#       define USE_GENERIC_PUSH_REGS
#       ifdef __ELF__
#            define DYNAMIC_LOADING
#	     include <features.h>
#	     if defined(__GLIBC__) && __GLIBC__ >= 2
#		 define LINUX_DATA_START
#	     else
     	         extern char **__environ;
#                define DATASTART ((ptr_t)(&__environ))
			      /* hideous kludge: __environ is the first */
			      /* word in crt0.o, and delimits the start */
			      /* of the data segment, no matter which   */
			      /* ld options were passed through.        */
			      /* We could use _etext instead, but that  */
			      /* would include .rodata, which may       */
			      /* contain large read-only data tables    */
			      /* that we'd rather not scan.		*/
#	     endif
	     extern int _end;
#	     define DATAEND (&_end)
#	else
	     extern int etext;
#            define DATASTART ((ptr_t)((((word) (&etext)) + 0xfff) & ~0xfff))
#       endif
#   endif
#   ifdef MSWINCE
#     define OS_TYPE "MSWINCE"
#     define DATAEND /* not needed */
#   endif
#endif

# ifdef SH
#   define MACH_TYPE "SH"
#   define ALIGNMENT 4
#   ifdef MSWINCE
#     define OS_TYPE "MSWINCE"
#     define DATAEND /* not needed */
#   endif
#   ifdef LINUX
#     define OS_TYPE "LINUX"
#     define STACKBOTTOM ((ptr_t) 0x7c000000)
#     define USE_GENERIC_PUSH_REGS
#     define DYNAMIC_LOADING
#     define LINUX_DATA_START
      extern int _end;
#     define DATAEND (&_end)
#   endif
# endif
 
# ifdef SH4
#   define MACH_TYPE "SH4"
#   define OS_TYPE "MSWINCE"
#   define ALIGNMENT 4
#   define DATAEND /* not needed */
# endif

#ifdef LINUX_DATA_START
    /* Some Linux distributions arrange to define __data_start.  Some	*/
    /* define data_start as a weak symbol.  The latter is technically	*/
    /* broken, since the user program may define data_start, in which	*/
    /* case we lose.  Nonetheless, we try both, prefering __data_start.	*/
    /* We assume gcc.	*/
#   pragma weak __data_start
    extern int __data_start;
#   pragma weak data_start
    extern int data_start;
#   define DATASTART ((ptr_t)(&__data_start != 0? &__data_start : &data_start))
#endif

#if defined(LINUX) && defined(REDIRECT_MALLOC)
    /* Rld appears to allocate some memory with its own allocator, and	*/
    /* some through malloc, which might be redirected.  To make this	*/
    /* work with collectable memory, we have to scan memory allocated	*/
    /* by rld's internal malloc.					*/
#   define USE_PROC_FOR_LIBRARIES
#endif
    
# ifndef STACK_GROWS_UP
#   define STACK_GROWS_DOWN
# endif

# ifndef CPP_WORDSZ
#   define CPP_WORDSZ 32
# endif

# ifndef OS_TYPE
#   define OS_TYPE ""
# endif

# ifndef DATAEND
    extern int end;
#   define DATAEND (&end)
# endif

# if defined(SVR4) && !defined(GETPAGESIZE)
#    include <unistd.h>
#    define GETPAGESIZE()  sysconf(_SC_PAGESIZE)
# endif

# ifndef GETPAGESIZE
#   if defined(SUNOS5) || defined(IRIX5)
#	include <unistd.h>
#   endif
#   define GETPAGESIZE() getpagesize()
# endif

# if defined(SUNOS5) || defined(DRSNX) || defined(UTS4)
    /* OS has SVR4 generic features.  Probably others also qualify.	*/
#   define SVR4
# endif

# if defined(SUNOS5) || defined(DRSNX)
    /* OS has SUNOS5 style semi-undocumented interface to dynamic 	*/
    /* loader.								*/
#   define SUNOS5DL
    /* OS has SUNOS5 style signal handlers.				*/
#   define SUNOS5SIGS
# endif

# if defined(HPUX)
#   define SUNOS5SIGS
# endif

# if defined(SVR4) || defined(LINUX) || defined(IRIX) || defined(HPUX) \
    || defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) \
    || defined(BSD) || defined(AIX) || defined(MACOSX) || defined(OSF1)
#   define UNIX_LIKE   /* Basic Unix-like system calls work.	*/
# endif

# if CPP_WORDSZ != 32 && CPP_WORDSZ != 64
   -> bad word size
# endif

# ifdef PCR
#   undef DYNAMIC_LOADING
#   undef STACKBOTTOM
#   undef HEURISTIC1
#   undef HEURISTIC2
#   undef PROC_VDB
#   undef MPROTECT_VDB
#   define PCR_VDB
# endif

# ifdef SRC_M3
/* Postponed for now. */
#   undef PROC_VDB
#   undef MPROTECT_VDB
# endif

# ifdef SMALL_CONFIG
/* Presumably not worth the space it takes. */
#   undef PROC_VDB
#   undef MPROTECT_VDB
# endif

# ifdef USE_MUNMAP
#   undef MPROTECT_VDB  /* Can't deal with address space holes. */
# endif

# ifdef PARALLEL_MARK
#   undef MPROTECT_VDB  /* For now.	*/
# endif

# if !defined(PCR_VDB) && !defined(PROC_VDB) && !defined(MPROTECT_VDB)
#   define DEFAULT_VDB
# endif

# ifndef PREFETCH
#   define PREFETCH(x)
#   define NO_PREFETCH
# endif

# ifndef PREFETCH_FOR_WRITE
#   define PREFETCH_FOR_WRITE(x)
#   define NO_PREFETCH_FOR_WRITE
# endif

# ifndef CACHE_LINE_SIZE
#   define CACHE_LINE_SIZE 32	/* Wild guess	*/
# endif

# ifndef CLEAR_DOUBLE
#   define CLEAR_DOUBLE(x) \
	((word*)x)[0] = 0; \
	((word*)x)[1] = 0;
# endif /* CLEAR_DOUBLE */

/* Internally to the collector we test only the XXX_THREADS macros	*/
/* not the GC_XXX_THREADS versions.  Here we make sure the latter	*/
/* are treated as equivalent.						*/
#if defined(GC_SOLARIS_THREADS) && !defined(_SOLARIS_THREADS)
#   define _SOLARIS_THREADS
#endif
#if defined(GC_SOLARIS_THREADS) && !defined(_SOLARIS_PTHREADS)
#   define _SOLARIS_PTHREADS
#endif
#if defined(GC_IRIX_THREADS) && !defined(IRIX_THREADS)
#   define IRIX_THREADS
#endif
#if defined(GC_LINUX_THREADS) && !defined(LINUX_THREADS)
#   define LINUX_THREADS
#endif
#if defined(GC_WIN32_THREADS) && !defined(WIN32_THREADS)
#   define WIN32_THREADS
#endif
#if defined(GC_HPUX_THREADS) && !defined(HPUX_THREADS)
#   define HPUX_THREADS
#endif
#if defined(GC_OSF1_THREADS) && !defined(OSF1_THREADS)
#   define OSF1_THREADS
#endif

/* Internally we use SOLARIS_THREADS to test for either old or pthreads. */
# if defined(_SOLARIS_PTHREADS) && !defined(SOLARIS_THREADS)
#   define SOLARIS_THREADS
# endif
# if defined(IRIX_THREADS) && !defined(IRIX5)
--> inconsistent configuration
# endif
# if defined(LINUX_THREADS) && !defined(LINUX)
--> inconsistent configuration
# endif
# if defined(SOLARIS_THREADS) && !defined(SUNOS5)
--> inconsistent configuration
# endif
# if defined(HPUX_THREADS) && !defined(HPUX)
--> inconsistent configuration
# endif
# if defined(PCR) || defined(SRC_M3) || \
	defined(SOLARIS_THREADS) || defined(WIN32_THREADS) || \
	defined(IRIX_THREADS) || defined(LINUX_THREADS) || \
	defined(HPUX_THREADS) || defined(OSF1_THREADS)
#   define THREADS
# endif

# if defined(HP_PA) || defined(M88K) || defined(POWERPC) && !defined(MACOSX) \
     || defined(LINT) || defined(MSWINCE) \
     || (defined(I386) && defined(__LCC__))
	/* Use setjmp based hack to mark from callee-save registers.    */
	/* The define should move to the individual platform 		*/
	/* descriptions.						*/
#	define USE_GENERIC_PUSH_REGS
# endif
# if defined(I386) && defined(LINUX)
    /* SAVE_CALL_CHAIN is supported if the code is compiled to save	*/
    /* frame pointers by default, i.e. no -fomit-frame-pointer flag.	*/
# ifdef SAVE_CALL_COUNT
#   define SAVE_CALL_CHAIN 
# endif
# endif
# if defined(SPARC)
#   define SAVE_CALL_CHAIN
#   define ASM_CLEAR_CODE	/* Stack clearing is crucial, and we 	*/
				/* include assembly code to do it well.	*/
# endif

# endif /* GCCONFIG_H */
