/* stdarg/vararg support for the Hitachi h8/300 and h8/300h */

/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef void *__gnuc_va_list;
#endif

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#ifdef __H8300__
#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#else
#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (long) - 1) / sizeof (long)) * sizeof (long))
#endif

#ifdef _STDARG_H

#define va_start(AP,LASTARG) \
  (AP = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))

#else /* _VARARGS_H */

#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#define va_start(AP)  AP = (void *) &__builtin_va_alist

#endif /* _VARARGS_H */

#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
  *((TYPE *) (void *) ((char *) (AP) - ((sizeof (TYPE) < 4		\
					 ? sizeof (TYPE)		\
					 : __va_rounded_size (TYPE))))))

#define va_end(AP)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
