/* GNU C varargs support for the Intel 80960.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
/* The first element is the address of the first argument.
   The second element is the number of bytes skipped past so far.  */
typedef unsigned __gnuc_va_list[2];	
#endif /* not __GNUC_VA_LIST */

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

/* The stack size of the type t.  */
#define __vsiz(T)   (((sizeof (T) + 3) / 4) * 4)
/* The stack alignment of the type t.  */
#define __vali(T)   (__alignof__ (T) >= 4 ? __alignof__ (T) : 4)
/* The offset of the next stack argument after one of type t at offset i.  */
#define __vpad(I, T) ((((I) + __vali (T) - 1) / __vali (T)) \
		       * __vali (T) + __vsiz (T))

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

#ifdef _STDARG_H
/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that firstarg is correct.  */
#define va_start(AP, LASTARG)				\
__extension__						\
({ __builtin_next_arg (LASTARG);			\
   __asm__ ("st	g14,%0" : "=m" (*(AP)));		\
   (AP)[1] = (__builtin_args_info (0) + __builtin_args_info (1)) * 4; })

#else

#define	va_alist __builtin_va_alist
#define	va_dcl	 char *__builtin_va_alist; __va_ellipsis
#define	va_start(AP) \
__extension__						\
({ __asm__ ("st	g14,%0" : "=m" (*(AP)));		\
   (AP)[1] = (__builtin_args_info (0) + __builtin_args_info (1)) * 4; })
#endif

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
#define	va_arg(AP, T)							\
(									\
  (									\
    ((AP)[1] <= 48 && (__vpad ((AP)[1], T) > 48 || __vsiz (T) > 16))	\
      ? ((AP)[1] = 48 + __vsiz (T))					\
      : ((AP)[1] = __vpad ((AP)[1], T))					\
  ),									\
									\
  *((T *) (void *) ((char *) *(AP) + (AP)[1] - __vsiz (T)))		\
)

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#endif
#define	va_end(AP)	((void *)0)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

