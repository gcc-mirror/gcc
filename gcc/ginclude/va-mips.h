/* ---------------------------------------- */
/*           VARARGS  for MIPS/GNU CC       */
/*                                          */
/*                                          */
/*                                          */
/*                                          */
/* ---------------------------------------- */


/* These macros implement varargs for GNU C--either traditional or ANSI.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
#if defined (__mips_eabi) && ! defined (__mips_soft_float) && ! defined (__mips_single_float)

typedef struct {
  /* Pointer to FP regs.  */
  char *__fp_regs;
  /* Number of FP regs remaining.  */
  int __fp_left;
  /* Pointer to GP regs followed by stack parameters.  */
  char *__gp_regs;
} __gnuc_va_list;

#else /* ! (defined (__mips_eabi) && ! defined (__mips_soft_float) && ! defined (__mips_single_float)) */

typedef char * __gnuc_va_list;

#endif /* ! (defined (__mips_eabi) && ! defined (__mips_soft_float) && ! defined (__mips_single_float)) */
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifndef _VA_MIPS_H_ENUM
#define _VA_MIPS_H_ENUM
enum {
  __no_type_class = -1,
  __void_type_class,
  __integer_type_class,
  __char_type_class,
  __enumeral_type_class,
  __boolean_type_class,
  __pointer_type_class,
  __reference_type_class,
  __offset_type_class,
  __real_type_class,
  __complex_type_class,
  __function_type_class,
  __method_type_class,
  __record_type_class,
  __union_type_class,
  __array_type_class,
  __string_type_class,
  __set_type_class,
  __file_type_class,
  __lang_type_class
};
#endif

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#ifdef __mips64
#define __va_rounded_size(__TYPE)  \
  (((sizeof (__TYPE) + 8 - 1) / 8) * 8)
#else
#define __va_rounded_size(__TYPE)  \
  (((sizeof (__TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#endif

#ifdef __mips64
#define __va_reg_size 8
#else
#define __va_reg_size 4
#endif

/* Get definitions for _MIPS_SIM_ABI64 etc.  */
#ifdef _MIPS_SIM
#include <sgidefs.h>
#endif

#ifdef _STDARG_H
#if defined (__mips_eabi)
#if ! defined (__mips_soft_float) && ! defined (__mips_single_float)
#ifdef __mips64
#define va_start(__AP, __LASTARG)					\
  (__AP.__gp_regs = ((char *) __builtin_next_arg (__LASTARG)		\
		     - (__builtin_args_info (2) < 8			\
			? (8 - __builtin_args_info (2)) * __va_reg_size	\
			: 0)),						\
   __AP.__fp_left = 8 - __builtin_args_info (3),			\
   __AP.__fp_regs = __AP.__gp_regs - __AP.__fp_left * __va_reg_size)
#else /* ! defined (__mips64) */
#define va_start(__AP, __LASTARG)					\
  (__AP.__gp_regs = ((char *) __builtin_next_arg (__LASTARG)		\
		     - (__builtin_args_info (2) < 8			\
			? (8 - __builtin_args_info (2)) * __va_reg_size	\
			: 0)),						\
   __AP.__fp_left = (8 - __builtin_args_info (3)) / 2,			\
   __AP.__fp_regs = __AP.__gp_regs - __AP.__fp_left * 8,		\
   __AP.__fp_regs = (char *) ((int) __AP.__fp_regs & -8))
#endif /* ! defined (__mips64) */
#else /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float) ) */
#define va_start(__AP, __LASTARG)					\
  (__AP = ((__gnuc_va_list) __builtin_next_arg (__LASTARG)		\
	   - (__builtin_args_info (2) >= 8 ? 0				\
	      : (8 - __builtin_args_info (2)) * __va_reg_size)))
#endif /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float) ) */
#else /* ! defined (__mips_eabi) */
#define va_start(__AP, __LASTARG) \
  (__AP = (__gnuc_va_list) __builtin_next_arg (__LASTARG))
#endif /* ! (defined (__mips_eabi) && ! defined (__mips_soft_float) && ! defined (__mips_single_float)) */
#else /* ! _STDARG_H */
#define va_alist  __builtin_va_alist
#ifdef __mips64
/* This assumes that `long long int' is always a 64 bit type.  */
#define va_dcl    long long int __builtin_va_alist; __va_ellipsis
#else
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#endif
#if defined (__mips_eabi)
#if ! defined (__mips_soft_float) && ! defined (__mips_single_float)
#ifdef __mips64
#define va_start(__AP)							\
  (__AP.__gp_regs = ((char *) __builtin_next_arg ()			\
		     - (__builtin_args_info (2) < 8			\
			? (8 - __builtin_args_info (2)) * __va_reg_size	\
			: __va_reg_size)),				\
   __AP.__fp_left = 8 - __builtin_args_info (3),			\
   __AP.__fp_regs = __AP.__gp_regs - __AP.__fp_left * __va_reg_size)
#else /* ! defined (__mips64) */
#define va_start(__AP)							\
  (__AP.__gp_regs = ((char *) __builtin_next_arg ()			\
		     - (__builtin_args_info (2) < 8			\
			? (8 - __builtin_args_info (2)) * __va_reg_size	\
			: __va_reg_size)),				\
   __AP.__fp_left = (8 - __builtin_args_info (3)) / 2,			\
   __AP.__fp_regs = __AP.__gp_regs - __AP.__fp_left * 8,		\
   __AP.__fp_regs = (char *) ((int) __AP.__fp_regs & -8))
#endif /* ! defined (__mips64) */
#else /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float)) */
#define va_start(__AP)							\
  (__AP = ((__gnuc_va_list) __builtin_next_arg ()			\
	   - (__builtin_args_info (2) >= 8 ? __va_reg_size		\
	      : (8 - __builtin_args_info (2)) * __va_reg_size)))
#endif /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float)) */
/* Need alternate code for _MIPS_SIM_ABI64.  */
#elif defined(_MIPS_SIM) && (_MIPS_SIM == _MIPS_SIM_ABI64 || _MIPS_SIM == _MIPS_SIM_NABI32)
#define va_start(__AP)							\
  (__AP = (__gnuc_va_list) __builtin_next_arg ()			\
   + (__builtin_args_info (2) >= 8 ? -8 : 0))
#else
#define va_start(__AP)  __AP = (char *) &__builtin_va_alist
#endif
#endif /* ! _STDARG_H */

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#endif
#define va_end(__AP)	((void)0)

#if defined (__mips_eabi)

#if ! defined (__mips_soft_float) && ! defined (__mips_single_float)
#ifdef __mips64
#define __va_next_addr(__AP, __type)					\
  ((__builtin_classify_type (*(__type *) 0) == __real_type_class	\
    && __AP.__fp_left > 0)						\
   ? (--__AP.__fp_left, (__AP.__fp_regs += 8) - 8)			\
   : (__AP.__gp_regs += __va_reg_size) - __va_reg_size)
#else
#define __va_next_addr(__AP, __type)					\
  ((__builtin_classify_type (*(__type *) 0) == __real_type_class	\
    && __AP.__fp_left > 0)						\
   ? (--__AP.__fp_left, (__AP.__fp_regs += 8) - 8)			\
   : (((__builtin_classify_type (* (__type *) 0) < __record_type_class	\
	&& __alignof__ (__type) > 4)					\
       ? __AP.__gp_regs = (char *) (((int) __AP.__gp_regs + 8 - 1) & -8) \
       : (char *) 0),							\
      (__builtin_classify_type (* (__type *) 0) >= __record_type_class	\
       ? (__AP.__gp_regs += __va_reg_size) - __va_reg_size		\
       : ((__AP.__gp_regs += __va_rounded_size (__type))		\
	  - __va_rounded_size (__type)))))
#endif
#else /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float)) */
#ifdef __mips64
#define __va_next_addr(__AP, __type)					\
  ((__AP += __va_reg_size) - __va_reg_size)
#else
#define __va_next_addr(__AP, __type)					\
  (((__builtin_classify_type (* (__type *) 0) < __record_type_class	\
     && __alignof__ (__type) > 4)					\
    ? __AP = (char *) (((int) __AP + 8 - 1) & -8)			\
    : (char *) 0),							\
   (__builtin_classify_type (* (__type *) 0) >= __record_type_class	\
    ? (__AP += __va_reg_size) - __va_reg_size				\
    : ((__AP += __va_rounded_size (__type))				\
       - __va_rounded_size (__type))))
#endif
#endif /* ! (! defined (__mips_soft_float) && ! defined (__mips_single_float)) */

#ifdef __MIPSEB__
#define va_arg(__AP, __type)						\
  ((__va_rounded_size (__type) <= __va_reg_size)			\
   ? *(__type *) (void *) (__va_next_addr (__AP, __type)		\
			   + __va_reg_size				\
			   - sizeof (__type))				\
   : (__builtin_classify_type (*(__type *) 0) >= __record_type_class	\
      ? **(__type **) (void *) (__va_next_addr (__AP, __type)		\
				+ __va_reg_size				\
				- sizeof (char *))			\
      : *(__type *) (void *) __va_next_addr (__AP, __type)))
#else
#define va_arg(__AP, __type)						\
  ((__va_rounded_size (__type) <= __va_reg_size)			\
   ? *(__type *) (void *) __va_next_addr (__AP, __type)		\
   : (__builtin_classify_type (* (__type *) 0) >= __record_type_class	\
      ? **(__type **) (void *) __va_next_addr (__AP, __type)		\
      : *(__type *) (void *) __va_next_addr (__AP, __type)))
#endif

#else /* ! defined (__mips_eabi) */

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
/* The __mips64 cases are reversed from the 32 bit cases, because the standard
   32 bit calling convention left-aligns all parameters smaller than a word,
   whereas the __mips64 calling convention does not (and hence they are
   right aligned).  */
#ifdef __mips64
#ifdef __MIPSEB__
#define va_arg(__AP, __type)                                    \
  ((__type *) (void *) (__AP = (char *) ((((__PTRDIFF_TYPE__)__AP + 8 - 1) & -8) \
					 + __va_rounded_size (__type))))[-1]
#else
#define va_arg(__AP, __type)                                    \
  ((__AP = (char *) ((((__PTRDIFF_TYPE__)__AP + 8 - 1) & -8)	\
		     + __va_rounded_size (__type))),		\
   *(__type *) (void *) (__AP - __va_rounded_size (__type)))
#endif

#else /* not __mips64 */

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
#endif /* ! defined (__mips_eabi)  */

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
