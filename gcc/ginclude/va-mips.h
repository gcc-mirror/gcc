/* ---------------------------------------- */
/*           VARARGS  for MIPS/GNU CC       */
/*                                          */
/*                                          */
/*                                          */
/*                                          */
/* ---------------------------------------- */


/* These macros implement varargs for GNU C--either traditional or ANSU.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef char * __gnuc_va_list;
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

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#ifdef _STDARG_H
#define va_start(AP, LASTARG) 						\
 (AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#else
#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#define va_start(AP)  AP = (char *) &__builtin_va_alist
#endif

void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#define va_end(AP)

#ifdef lint	/* complains about constant in conditional context */
#define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]

#else		/* !lint */
#define va_arg(AP, mode)						\
  ((mode *)(AP = (char *) (__alignof(mode) > 4				\
				? ((int)AP + 2*8 - 1) & -8		\
				: ((int)AP + 2*4 - 1) & -4)))[-1]
#endif		/* lint */

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
