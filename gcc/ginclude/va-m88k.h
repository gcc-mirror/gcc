/* GNU C varargs support for the Motorola 88100  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct
{
  int  __va_arg;		/* argument number */
  int *__va_stk;		/* start of args passed on stack */
  int *__va_reg;		/* start of args passed in regs */
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H /* stdarg.h support */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that LASTARG is correct.  */
#if __GNUC__ > 1 /* GCC 2.0 and beyond */
#define va_start(AP,LASTARG)				\
 (__builtin_next_arg (LASTARG),				\
  (AP) = *(__gnuc_va_list *)__builtin_saveregs())
#else
#define va_start(AP,LASTARG) \
  ( (AP).__va_reg = (int *) __builtin_saveregs2(0), \
    (AP).__va_stk = (int *) __builtin_argptr(), \
    (AP).__va_arg = (int) (__builtin_argsize() + 3) / 4 )
#endif

#else /* varargs.h support */

#if __GNUC__ > 1 /* GCC 2.0 and beyond */
#define va_start(AP) ((AP) = *(__gnuc_va_list *)__builtin_saveregs())
#else
#define va_start(AP) \
  ( (AP).__va_reg = (int *) __builtin_saveregs2(1), \
    (AP).__va_stk = (int *) __builtin_argptr(), \
    (AP).__va_arg = (int) (__builtin_argsize() - 4 + 3) / 4 )
#endif
#define va_alist __va_1st_arg
#define va_dcl register int va_alist;...

#endif /* _STDARG_H */

/* Avoid trouble between this file and _int_varargs.h under DG/UX.  This file
   can be included by <stdio.h> and others and provides definitions of
   __va_size and __va_reg_p and  a va_list typedef.  Avoid defining va_list
   again with _VA_LIST.  */
#ifdef __INT_VARARGS_H
#undef __va_size
#undef __va_reg_p
#define __gnuc_va_list va_list
#define _VA_LIST
#define _VA_LIST_
#else
/* Similarly, if this gets included first, do nothing in _int_varargs.h.  */
#define __INT_VARARGS_H
#endif

#define __va_reg_p(TYPE) \
  (__builtin_classify_type(*(TYPE *)0) < 12 \
   ? sizeof(TYPE) <= 8 : sizeof(TYPE) == 4 && __alignof__(TYPE) == 4)

#define	__va_size(TYPE) ((sizeof(TYPE) + 3) >> 2)

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
#define va_arg(AP,TYPE)							   \
  ( (AP).__va_arg = (((AP).__va_arg + (1 << (__alignof__(TYPE) >> 3)) - 1) \
		     & ~((1 << (__alignof__(TYPE) >> 3)) - 1))		   \
    + __va_size(TYPE),							   \
    *((TYPE *) (void *) ((__va_reg_p(TYPE)				   \
			  && (AP).__va_arg < 8 + __va_size(TYPE)	   \
			  ? (AP).__va_reg : (AP).__va_stk)		   \
			 + ((AP).__va_arg - __va_size(TYPE)))))

#define va_end(AP)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
