/* Note:  We must use the name __builtin_savregs.  GCC attaches special
   significance to that name.  In particular, regardless of where in a
   function __builtin_saveregs is called, GCC moves the call up to the
   very start of the function.  */


/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef union {
  float		__freg[8];
  double	__dreg[4];
} __f_regs;

typedef struct {
#if defined (__SVR4__) || defined (__svr4__) || defined (__alliant__) || defined (__PARAGON__)
  __f_regs __float_regs; long __ireg[12];
#else /* pre-SVR4 */
  long __ireg[12]; __f_regs __float_regs;
#endif
} __va_saved_regs;

typedef struct {
#if defined(__SVR4__) || defined(__svr4__) || defined(__alliant__) || defined (__PARAGON__)
  unsigned	__ireg_used;	/* How many int regs consumed 'til now? */
  unsigned	__freg_used;	/* How many flt regs consumed 'til now? */
  long		*__reg_base;	/* Address of where we stored the regs. */
  long *	__mem_ptr;	/* Address of memory overflow args area. */
#else /* pre-SVR4 */
  long		*__reg_base;	/* Address of where we stored the regs. */
  long *	__mem_ptr;	/* Address of memory overflow args area. */
  unsigned	__ireg_used;	/* How many int regs consumed 'til now? */
  unsigned	__freg_used;	/* How many flt regs consumed 'til now? */
#endif
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#if !defined(_STDARG_H)

/* varargs support */
#define va_alist __builtin_va_alist
#if defined (__PARAGON__)
#define va_dcl int va_alist;
#else	/* __PARAGON__ */
#define va_dcl
#endif	/* __PARAGON__ */
#define va_start(pvar) ((pvar) = * (__gnuc_va_list *) __builtin_saveregs ())

#else /* STDARG.H */

/* ANSI alternative.  */
/* Note that CUMULATIVE_ARGS elements are measured in bytes on the i860,
   so we divide by 4 to get # of registers.  */
#define va_start(pvar, firstarg) \
 ((pvar) = *(__gnuc_va_list *) __builtin_saveregs (),			\
  (pvar).__ireg_used = __builtin_args_info (0) / 4,		\
  (pvar).__freg_used = __builtin_args_info (1) / 4,		\
  (pvar).__mem_ptr = __builtin_next_arg (firstarg))

#endif /* _STDARG_H */

/* Values returned by __builtin_classify_type.  */

#ifndef va_end
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

void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#endif
#define va_end(__va)	((void) 0)

#define __NUM_PARM_FREGS	8
#define __NUM_PARM_IREGS	12

#define __savereg(__va) ((__va_saved_regs *) ((__va).__reg_base))

/* This macro works both for SVR4 and pre-SVR4 environments.  */

/* Note that parameters are always aligned at least to a word boundary
   (when passed) regardless of what GCC's __alignof__ operator says.  */

/* Make allowances here for adding 128-bit (long double) floats someday.  */

#if 0 /* What was this for? */
#ifndef __GNU_VA_LIST
#define __ireg_used ireg_used
#define __freg_used freg_used
#define __mem_ptr mem_ptr
#define __reg_base reg_base
#endif
#endif /* 0 */

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

#define va_arg(__va, __type)						\
__extension__								\
(* (__type *)								\
({									\
  register void *__rv;  /* result value */				\
  register unsigned __align;						\
  switch (__builtin_classify_type (* (__type *) 0))			\
    {									\
    case __real_type_class:						\
      switch (sizeof (__type))						\
	{								\
	  case sizeof (float):						\
	  case sizeof (double):						\
	    if ((__va).__freg_used < __NUM_PARM_FREGS - 1)		\
	      {								\
	        if (((__va).__freg_used & 1) != 0)			\
	          (__va).__freg_used++;	/* skip odd */			\
	        __rv = &__savereg((__va))->__float_regs.__freg[(__va).__freg_used];\
		(__va).__freg_used += 2;				\
	      }								\
	    else							\
	      {								\
	        if ((((unsigned) (__va).__mem_ptr) & (sizeof(double)-1)) != 0) \
	          (__va).__mem_ptr++;	/* skip odd */			\
	        __rv = (__va).__mem_ptr;				\
	        (__va).__mem_ptr += 2;					\
	      }								\
	    if (sizeof (__type) == sizeof (float))			\
	      {								\
	        *((float *) __rv) = *((double *) __rv);			\
		*(((long *) __rv) + 1) = 0xfff00001;			\
	      }								\
	    break;							\
	  default:							\
	    abort ();							\
	}								\
      break;								\
    case __void_type_class:						\
    case __integer_type_class:						\
    case __char_type_class:						\
    case __enumeral_type_class:						\
    case __boolean_type_class:						\
    case __pointer_type_class:						\
    case __reference_type_class:					\
    case __offset_type_class:						\
      if (sizeof (__type) <= 4)						\
	{								\
          __rv = ((__va).__ireg_used < __NUM_PARM_IREGS			\
	          ? (&__savereg((__va))->__ireg[(__va).__ireg_used++])	\
	          : (__va).__mem_ptr++);				\
	  break;							\
	}								\
      else if ((__va).__ireg_used + sizeof (__type) / 4 <= __NUM_PARM_IREGS) \
	{								\
	  __rv = &__savereg((__va))->__ireg[(__va).__ireg_used];	\
	  (__va).__ireg_used += sizeof (__type) / 4;			\
          break;							\
	}								\
      /* Fall through to fetch from memory.  */				\
    case __record_type_class:						\
    case __union_type_class:						\
      __align = (__alignof__ (__type) < sizeof (long)			\
		 ? sizeof (long)					\
		 : __alignof__ (__type));				\
      (__va).__mem_ptr							\
	= (long *)							\
	  ((((unsigned) (__va).__mem_ptr) + (__align-1)) & ~(__align-1)); \
      __rv = (__va).__mem_ptr;						\
      (__va).__mem_ptr							\
	+= ((sizeof (__type) + sizeof (long) - 1) / sizeof (long));	\
      break;								\
    case __complex_type_class:						\
    case __function_type_class:						\
    case __method_type_class:						\
    case __array_type_class:						\
    case __string_type_class:						\
    case __set_type_class:						\
    case __file_type_class:						\
    case __lang_type_class:						\
    case __no_type_class:						\
    default:								\
	abort ();							\
    }									\
  __rv;									\
}))

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

