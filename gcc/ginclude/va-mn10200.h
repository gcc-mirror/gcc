/* CYGNUS LOCAL entire file/law */
/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef void *__gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)
#define __gnuc_va_start(AP) (AP = (__gnuc_va_list)__builtin_saveregs())
#define __va_ellipsis ...

#ifdef _STDARG_H
#define va_start(AP, LASTARG) \
 (AP = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))
#else
#define va_alist __builtin_va_alist
#define va_dcl int __builtin_va_alist; __va_ellipsis
#define va_start(AP)  AP=(char *) &__builtin_va_alist
#endif

/* Now stuff common to both varargs & stdarg implementations.  */
#define __va_rounded_size(TYPE)						\
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#undef va_end
void va_end (__gnuc_va_list);
#define va_end(AP) ((void)0)
#define va_arg(AP, TYPE)						\
 (sizeof (TYPE) > 8							\
  ? (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (char *)),\
    **((TYPE **) (void *) ((char *) (AP) - __va_rounded_size (char *))))\
  : (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
    *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (TYPE)))))
#endif
/* END CYGNUS LOCAL */
