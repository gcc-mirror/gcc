#ifndef __GNUC__
/* Use the system's macros with the system's compiler.  */
#include <varargs.h>
#else
/* Record that varargs.h is defined; this turns off stdarg.h.  */

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

#ifdef _HIDDEN_VA_LIST  /* On OSF1, this means varargs.h is "half-loaded".  */
#undef _VA_LIST
#endif

/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.  */
/* But in 4.3bsd-net2, _VA_LIST_ has another meaning.  So ignore it.  */
#if !defined (_VA_LIST_) || defined (_ANSI_H_)
/* The macro _VA_LIST is used in SCO Unix 3.2.  */
#ifndef _VA_LIST
#ifndef _VA_LIST_
#define _VA_LIST_
#endif
#define _VA_LIST
/* Make this a macro rather than a typedef, so we can undef any other defn.  */
#define va_list __va___list
typedef char * __va___list;
#endif /* _VA_LIST */
#endif /* !defined (_VA_LIST_) || defined (_ANSI_H_) */

/*  In 4.3bsd-net2, it is said we must #undef this.
    I hope this successfully identifies that system.
    I don't know why this works--rms.  */
#ifdef _ANSI_H_
#undef _VA_LIST_
#endif

#define va_start(AP)  AP=(char *) &__builtin_va_alist

#define va_end(AP)

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define va_arg(AP, TYPE)						\
 (AP += __va_rounded_size (TYPE),					\
  *((TYPE *) (AP - __va_rounded_size (TYPE))))

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

#ifdef __GNUC_VA_LIST
/* If an include file defined __GNUC_VA_LIST,
   copy it into va_list.  */

#ifdef _HIDDEN_VA_LIST  /* On OSF1, this means varargs.h is "half-loaded".  */
#undef _VA_LIST
#endif

/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.  */
#ifndef _VA_LIST_
/* The macro _VA_LIST is used in SCO Unix 3.2.  */
#ifndef _VA_LIST
#define _VA_LIST_
#define _VA_LIST
typedef __gnuc_va_list va_list;
#endif /* _VA_LIST */
#endif /* _VA_LIST_ */
#endif /* __GNUC_VA_LIST */

#endif /* __GNUC__ */
