#if __GNUC__ > 1

/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef double *__gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)
#ifdef _STDARG_H
#define va_start(AP,LASTARG) ((AP) = (va_list)__builtin_saveregs())
#else
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl long va_alist; ...
#define va_start(AP) ((AP) = (va_list)__builtin_saveregs())
#endif

#define va_arg(AP,TYPE)						\
  (sizeof(TYPE) > 8 ?						\
   ((AP = (__gnuc_va_list) ((char *)AP - sizeof (int))),	\
    (*((TYPE *) (*((int *) (AP))))))				\
   :((AP =							\
      (__gnuc_va_list) ((long)((char *)AP - sizeof (TYPE))	\
			& (sizeof(TYPE) > 4 ? ~0x7 : ~0x3))),	\
     (*((TYPE *) ((char *)AP + ((8 - sizeof(TYPE)) % 4))))))

#define va_end(AP)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

#else /* not __GNUC__ > 1 */
#include "/usr/local/lib/gcc-include/va-hp9k8.h"
#define _VA_LIST_
#endif
