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

#if __mips==3
#define __va_rounded_size(__TYPE)  \
  (((sizeof (__TYPE) + 8 - 1) / 8) * 8)
#else
#define __va_rounded_size(__TYPE)  \
  (((sizeof (__TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#endif

#ifdef _STDARG_H
#define va_start(__AP, __LASTARG) \
  (__AP = (__gnuc_va_list) __builtin_next_arg (__LASTARG))

#else
#define va_alist  __builtin_va_alist
#if __mips==3
/* This assumes that `long long int' is always a 64 bit type.  */
#define va_dcl    long long int __builtin_va_alist; __va_ellipsis
#else
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#endif
#define va_start(__AP)  __AP = (char *) &__builtin_va_alist
#endif

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#endif
#define va_end(__AP)

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
/* The __mips==3 cases are reversed from the 32 bit cases, because the standard
   32 bit calling convention left-aligns all parameters smaller than a word,
   whereas the __mips==3 calling convention does not (and hence they are
   right aligned).  */
#if __mips==3
#ifdef __MIPSEB__
#define va_arg(__AP, __type)                                    \
  ((__type *) (void *) (__AP = (char *) ((((int)__AP + 8 - 1) & -8) \
					 + __va_rounded_size (__type))))[-1]
#else
#define va_arg(__AP, __type)                                    \
  ((__AP = (char *) ((((int)__AP + 8 - 1) & -8)			\
		     + __va_rounded_size (__type))),		\
   *(__type *) (void *) (__AP - __va_rounded_size (__type)))
#endif

#else /* not __mips==3 */

#ifdef __MIPSEB__
/* For big-endian machines.  */
#define va_arg(__AP, __type)					\
  ((__AP = (char *) ((__alignof__ (__type) > 4			\
		      ? ((int)__AP + 8 - 1) & -8		\
		      : ((int)__AP + 4 - 1) & -4)		\
		     + __va_rounded_size (__type))),		\
   *(__type *) (void *) (__AP - __va_rounded_size (__type)))
#else
/* For little-endian machines.  */
#define va_arg(__AP, __type)						    \
  ((__type *) (void *) (__AP = (char *) ((__alignof__(__type) > 4	    \
					  ? ((int)__AP + 8 - 1) & -8	    \
					  : ((int)__AP + 4 - 1) & -4)	    \
					 + __va_rounded_size(__type))))[-1]
#endif
#endif

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
