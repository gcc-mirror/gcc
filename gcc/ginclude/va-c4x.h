/* GNU C varargs support for the TMS320C[34]x  */

/* C[34]x arguments grow in weird ways (downwards) that the standard
   varargs stuff can't handle. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef void *__gnuc_va_list;

#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H /* stdarg.h support */

#define va_start(AP,LASTARG) AP=(__gnuc_va_list) __builtin_next_arg (LASTARG)

#else /* varargs.h support */

#define	__va_ellipsis	...
#define	va_alist	__builtin_va_alist
#define	va_dcl		int __builtin_va_alist; __va_ellipsis
#define va_start(AP)	AP=(__gnuc_va_list) ((int *)&__builtin_va_alist +  1)

#endif /* _STDARG_H */

#define va_end(AP)	((void) 0)
#define va_arg(AP,TYPE)	(AP = (__gnuc_va_list) ((char *) (AP) - sizeof(TYPE)), \
			 *((TYPE *) ((char *) (AP))))

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
