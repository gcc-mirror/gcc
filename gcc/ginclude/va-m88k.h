/* This file contains changes made by Data General, December 1989.  */
/* GNU C varargs support for the Motorola 88100  */

#ifndef __INT_VARARGS_H		/* Prevent multiple inclusions of this file */
#define __INT_VARARGS_H		/* and _int_varargs.h under DG/UX */

typedef struct
{
  int  __va_arg;		/* argument number */
  int *__va_stk;		/* start of args passed on stack */
  int *__va_reg;		/* start of args passed in regs */
} va_list;

#else
#undef __va_size
#undef __va_reg_p
#endif /* __INT_VARARGS_H */

#ifdef _STDARG_H /* stdarg.h support */

#if __GNUC__ > 1 /* GCC 2.0 and beyond */
#define va_start(AP,LASTARG) ((AP) = *(va_list *)__builtin_saveregs())
#else
#define va_start(AP,LASTARG) \
  ( (AP).__va_reg = (int *) __builtin_saveregs2(0), \
    (AP).__va_stk = (int *) __builtin_argptr(), \
    (AP).__va_arg = (int) (__builtin_argsize() + 3) / 4 )
#endif

#else /* varargs.h support */

#if __GNUC__ > 1 /* GCC 2.0 and beyond */
#define va_start(AP) ((AP) = *(va_list *)__builtin_saveregs())
#else
#define va_start(AP) \
  ( (AP).__va_reg = (int *) __builtin_saveregs2(1), \
    (AP).__va_stk = (int *) __builtin_argptr(), \
    (AP).__va_arg = (int) (__builtin_argsize() - 4 + 3) / 4 )
#endif
#define va_alist __va_1st_arg
#define va_dcl register int va_alist;

#endif /* _STDARG_H */

#define __va_reg_p(TYPE) \
  (__builtin_classify_type(*(TYPE *)0) < 12 \
   ? sizeof(TYPE) <= 8 : sizeof(TYPE) == 4 && __alignof__(TYPE) == 4)

#define	__va_size(TYPE) ((sizeof(TYPE) + 3) >> 2)

#define va_arg(AP,TYPE) \
  ( (AP).__va_arg = (((AP).__va_arg + (1 << (__alignof__(TYPE) >> 3)) - 1) \
		     & ~((1 << (__alignof__(TYPE) >> 3)) - 1)) \
    + __va_size(TYPE), \
    *((TYPE *) ((__va_reg_p(TYPE) && (AP).__va_arg < 8 + __va_size(TYPE) \
		 ? (AP).__va_reg : (AP).__va_stk) \
		+ ((AP).__va_arg - __va_size(TYPE)))))

#define va_end(AP)
