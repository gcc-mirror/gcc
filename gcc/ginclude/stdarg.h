/* stdarg.h for GNU.
   Note that the type used in va_arg is supposed to match the
   actual type **after default promotions**.
   Thus, va_arg (..., short) is not valid.  */

#ifndef _STDARG_H
#ifndef _ANSI_STDARG_H_
#ifndef __need___va_list
#define _STDARG_H
#define _ANSI_STDARG_H_
#endif /* not __need___va_list */
#undef __need___va_list

#ifndef __GNUC__
/* Use the system's macros with the system's compiler.
   This is relevant only when building GCC with some other compiler.  */
#include <stdarg.h>
#else
#ifdef __m88k__
#include <va-m88k.h>
#else
#ifdef __i860__
#include <va-i860.h>
#else
#ifdef __hppa__
#include <va-pa.h>
#else
#ifdef __mips__
#include <va-mips.h>
#else
#ifdef __sparc__
#include <va-sparc.h>
#else
#ifdef __i960__
#include <va-i960.h>
#else
#ifdef __alpha__
#include <va-alpha.h>
#else

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
#if defined(__svr4__) || defined(_AIX) || defined(_M_UNIX)
typedef char *__gnuc_va_list;
#else
typedef void *__gnuc_va_list;
#endif
#endif

/* Define the standard macros for the user,
   if this invocation was from the user program.  */
#ifdef _STDARG_H

/* Amount of space required in an argument list for an arg of type TYPE.
   TYPE may alternatively be an expression whose type is used.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define va_start(AP, LASTARG) 						\
 (AP = ((__gnuc_va_list) __builtin_next_arg ()))

#undef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#define va_end(AP)

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */

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
#endif /* _STDARG_H */

#endif /* not alpha */
#endif /* not i960 */
#endif /* not sparc */
#endif /* not mips */
#endif /* not hppa */
#endif /* not i860 */
#endif /* not m88k */

#ifdef _STDARG_H
/* Define va_list, if desired, from __gnuc_va_list. */
/* We deliberately do not define va_list when called from
   stdio.h, because ANSI C says that stdio.h is not supposed to define
   va_list.  stdio.h needs to have access to that data type, 
   but must not use that name.  It should use the name __gnuc_va_list,
   which is safe because it is reserved for the implementation.  */

#ifdef _HIDDEN_VA_LIST  /* On OSF1, this means varargs.h is "half-loaded".  */
#undef _VA_LIST
#endif

#ifdef _BSD_VA_LIST
#undef _BSD_VA_LIST
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

#endif /* _STDARG_H */

#endif /* __GNUC__ */
#endif /* not _ANSI_STDARG_H_ */
#endif /* not _STDARG_H */
