/* ---------------------------------------- */
/*           VARARGS  for MIPS/GNU CC       */
/*                                          */
/*                                          */
/*                                          */
/*                                          */
/* ---------------------------------------- */


/* These macros implement traditional (non-ANSI) varargs
   for GNU C.  */

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#ifndef _VA_LIST_
#define _VA_LIST_
/* Make this a macro rather than a typedef, so we can undef any other defn.  */
#define va_list __va___list
typedef char * __va___list;
#endif

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#ifdef _STDARG_H
#define va_start(AP, LASTARG) 						\
 (AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#else
#define va_start(AP)  AP = (char *) &__builtin_va_alist
#endif

#define va_end(AP)

#ifdef lint	/* complains about constant in conditional context */
#define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]

#else		/* !lint */
#define va_arg(AP, mode)						\
  ((mode *)(AP = (char *) (__alignof(mode) > 4				\
				? ((int)AP + 2*8 - 1) & -8		\
				: ((int)AP + 2*4 - 1) & -4)))[-1]
#endif		/* lint */

