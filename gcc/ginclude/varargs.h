#ifndef __GNUC__
/* Use the system's macros with the system's compiler.  */
#include <varargs.h>
#else
/* Record that this is varargs.h; this turns off stdarg.h.  */

#ifndef _VARARGS_H
#define _VARARGS_H

#ifdef __sparc__
#include <va-sparc.h>
#else
#ifdef __spur__
#include <va-spur.h>
#else
#ifdef __mips__
#include <va-mips.h>
#else
#ifdef __i860__
#include <va-i860.h>
#else
#ifdef __pyr__
#include <va-pyr.h>
#else
#ifdef __m88k__
#include <va-m88k.h>
#else
#if defined(__hppa__) || defined(hp800)
#include <va-pa.h>
#else
#ifdef __i960__
#include <va-i960.h>
#else
#ifdef __alpha__
#include <va-alpha.h>
#else

#ifdef __NeXT__

/* On Next, erase any vestiges of stdarg.h.  */

#ifdef _ANSI_STDARG_H_
#define _VA_LIST_
#endif
#define _ANSI_STDARG_H_ 

#undef va_alist
#undef va_dcl
#undef va_list
#undef va_start
#undef va_end
#undef __va_rounded_size
#undef va_arg
#endif  /* __NeXT__ */

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

/* These macros implement traditional (non-ANSI) varargs
   for GNU C.  */

#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; __va_ellipsis

/* Define __gnuc_va_list, just as in gstdarg.h.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
#if defined(__svr4__) || defined(_AIX) || defined(_M_UNIX)
typedef char *__gnuc_va_list;
#else
typedef void *__gnuc_va_list;
#endif
#endif

#define va_start(AP)  AP=(char *) &__builtin_va_alist

#define va_end(AP)

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#if defined (__arm__) || defined (__i386__) || defined (__ns32000__) || defined (__vax__)
/* This is for little-endian machines; small args are padded upward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
  *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (TYPE))))
#else /* big-endian */
/* This is for big-endian machines; small args are padded downward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
  *((TYPE *) (void *) ((char *) (AP) - ((sizeof (TYPE) < 4		\
					 ? sizeof (TYPE)		\
					 : __va_rounded_size (TYPE))))))
#endif /* big-endian */

#endif /* not alpha */
#endif /* not i960 */
#endif /* not hppa */
#endif /* not m88k */
#endif /* not pyr */
#endif /* not i860 */
#endif /* not mips */
#endif /* not spur */
#endif /* not sparc */
#endif /* not _VARARGS_H */

/* Define va_list from __gnuc_va_list.  */

#ifdef _HIDDEN_VA_LIST  /* On OSF1, this means varargs.h is "half-loaded".  */
#undef _VA_LIST
#endif

#ifdef __SVR4_2__

/* SVR4.2 uses _VA_LIST for an internal alias for va_list,
   so we must avoid testing it and setting it here.  */
#ifndef _VA_LIST_
#define _VA_LIST_
typedef __gnuc_va_list va_list;
#endif /* _VA_LIST_ */

#else /* not __SVR4_2__ */

/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.
   But on BSD NET2 we must not test or define or undef it.
   (Note that the comments in NET 2's ansi.h
   are incorrect for _VA_LIST_--see stdio.h!)  */
#if !defined (_VA_LIST_) || defined (__BSD_NET2__) || defined (____386BSD____)
/* The macro _VA_LIST is used in SCO Unix 3.2.  */
#ifndef _VA_LIST
#if !(defined (__BSD_NET2__) || defined (____386BSD____))
#define _VA_LIST_
#endif
#define _VA_LIST
typedef __gnuc_va_list va_list;
#endif /* not _VA_LIST */
#endif /* not _VA_LIST_ */

#endif /* not __SVR4_2__ */

/* The next BSD release (if there is one) wants this symbol to be
   undefined instead of _VA_LIST_.  */
#ifdef _BSD_VA_LIST
#undef _BSD_VA_LIST
#endif

#endif /* __GNUC__ */
