/* stdarg.h for GNU.
   Note that the type used in va_arg is supposed to match the
   actual type **after default promotions**.
   Thus, va_arg (..., short) is not valid.  */

#ifndef _STDARG_H
#ifndef __need___va_list
#define _STDARG_H
#endif
#undef __need___va_list

#ifndef __GNUC__
/* Use the system's macros with the system's compiler.  */
#include <stdarg.h>
#else
#ifdef __m88k__
#include "va-m88k.h"
#else
#ifdef __i860__
#include "va-i860.h"
#else
#ifdef __hp9000s800__
#include "va-pa.h"
#else
#ifdef __mips__
#include "va-mips.h"
#else
#ifdef __sparc__
#include "va-sparc.h"
#else
#ifdef __i960__
#include "va-i960.h"
#else

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
#ifdef __svr4__
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
 (AP = ((char *) __builtin_next_arg ()))

void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#define va_end(AP)

#define va_arg(AP, TYPE)						\
 (AP = ((char *) (AP)) += __va_rounded_size (TYPE),			\
  *((TYPE *) ((char *) (AP) - __va_rounded_size (TYPE))))
#endif /* _STDARG_H */

#endif /* not i960 */
#endif /* not sparc */
#endif /* not mips */
#endif /* not hp9000s800 */
#endif /* not i860 */
#endif /* not m88k */

#ifdef _STDARG_H
/* Define va_list, if desired, from __gnuc_va_list. */

#ifdef _HIDDEN_VA_LIST  /* On OSF1, this means varargs.h is "half-loaded".  */
#undef _VA_LIST
#endif

#ifdef _BSD_VA_LIST
#undef _BSD_VA_LIST
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
#endif /* _STDARG_H */

#endif /* __GNUC__ */
#endif /* not _STDARG_H */
