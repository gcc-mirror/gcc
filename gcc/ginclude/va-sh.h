/* The ! __SH3E_VARG case is similar to the default gvarargs.h .  */

#if (defined (__SH3E__) || defined (__SH4_SINGLE__) || defined (__SH4__) || defined (__SH4_SINGLE_ONLY__)) && ! defined (__HITACHI__)
#define __SH3E_VARG
#endif

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

#ifdef __SH3E_VARG

typedef long __va_greg;
typedef float __va_freg;

typedef struct {
  __va_greg * __va_next_o;		/* next available register */
  __va_greg * __va_next_o_limit;	/* past last available register */
  __va_freg * __va_next_fp;		/* next available fp register */
  __va_freg * __va_next_fp_limit;	/* last available fp register */
  __va_greg * __va_next_stack;		/* next extended word on stack */
} __gnuc_va_list;

#else /* ! SH3E */

typedef void *__gnuc_va_list;

#endif /* ! SH3E */

#endif /* __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H

#ifdef __SH3E_VARG

#define va_start(AP, LASTARG) \
__extension__ \
  ({ \
     (AP).__va_next_fp = (__va_freg *) __builtin_saveregs (); \
     (AP).__va_next_fp_limit = ((AP).__va_next_fp + \
			      (__builtin_args_info (1) < 8 ? 8 - __builtin_args_info (1) : 0)); \
     (AP).__va_next_o = (__va_greg *) (AP).__va_next_fp_limit; \
     (AP).__va_next_o_limit = ((AP).__va_next_o + \
			     (__builtin_args_info (0) < 4 ? 4 - __builtin_args_info (0) : 0)); \
     (AP).__va_next_stack = (__va_greg *) __builtin_next_arg (LASTARG); \
  })

#else /* ! SH3E */

#define va_start(AP, LASTARG) 						\
 ((AP) = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))

#endif /* ! SH3E */

#else /* _VARARGS_H */

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist;...

#ifdef __SH3E_VARG

#define va_start(AP) \
__extension__ \
  ({ \
     (AP).__va_next_fp = (__va_freg *) __builtin_saveregs (); \
     (AP).__va_next_fp_limit = ((AP).__va_next_fp + \
			      (__builtin_args_info (1) < 8 ? 8 - __builtin_args_info (1) : 0)); \
     (AP).__va_next_o = (__va_greg *) (AP).__va_next_fp_limit; \
     (AP).__va_next_o_limit = ((AP).__va_next_o + \
			     (__builtin_args_info (0) < 4 ? 4 - __builtin_args_info (0) : 0)); \
     (AP).__va_next_stack \
       = ((__va_greg *) __builtin_next_arg (__builtin_va_alist) \
	  - (__builtin_args_info (0) >= 4 || __builtin_args_info (1) >= 8 \
	     ? 1 : 0)); \
  })

#else /* ! SH3E */

#define va_start(AP)  ((AP) = (char *) &__builtin_va_alist)

#endif /* ! SH3E */

#endif /* _STDARG */

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */

/* Values returned by __builtin_classify_type.  */

enum __va_type_classes {
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
#define va_end(pvar)	((void)0)

#ifdef __LITTLE_ENDIAN__
#define __LITTLE_ENDIAN_P 1
#else
#define __LITTLE_ENDIAN_P 0
#endif

#define __SCALAR_TYPE(TYPE)					\
  ((TYPE) == __integer_type_class				\
   || (TYPE) == __char_type_class				\
   || (TYPE) == __enumeral_type_class)

/* RECORD_TYPE args passed using the C calling convention are
   passed by invisible reference.  ??? RECORD_TYPE args passed
   in the stack are made to be word-aligned; for an aggregate that is
   not word-aligned, we advance the pointer to the first non-reg slot.  */

  /* When this is a smaller-than-int integer, using
     auto-increment in the promoted (SImode) is fastest;
     however, there is no way to express that is C.  Therefore,
     we use an asm.
     We want the MEM_IN_STRUCT_P bit set in the emitted RTL, therefore we
     use unions even when it would otherwise be unnecessary.  */

/* gcc has an extension that allows to use a casted lvalue as an lvalue,
   But it doesn't work in C++ with -pedantic - even in the presence of
   __extension__ .  We work around this problem by using a reference type.  */
#ifdef __cplusplus
#define __VA_REF &
#else
#define __VA_REF
#endif

#define __va_arg_sh1(AP, TYPE) __extension__ 				\
({(sizeof (TYPE) == 1							\
   ? ({union {TYPE t; char c;} __t;					\
       __asm(""								\
	     : "=r" (__t.c)						\
	     : "0" ((((union { int i, j; } *__VA_REF) (AP))++)->i));	\
       __t.t;})								\
   : sizeof (TYPE) == 2							\
   ? ({union {TYPE t; short s;} __t;					\
       __asm(""								\
	     : "=r" (__t.s)						\
	     : "0" ((((union { int i, j; } *__VA_REF) (AP))++)->i));	\
       __t.t;})								\
   : sizeof (TYPE) >= 4 || __LITTLE_ENDIAN_P				\
   ? (((union { TYPE t; int i;} *__VA_REF) (AP))++)->t			\
   : ((union {TYPE t;TYPE u;}*) ((char *)++(int *__VA_REF)(AP) - sizeof (TYPE)))->t);})

#ifdef __SH3E_VARG

#define __PASS_AS_FLOAT(TYPE_CLASS,SIZE) \
  (TYPE_CLASS == __real_type_class && SIZE == 4)

#define __TARGET_SH4_P 0

#if defined(__SH4__) || defined(__SH4_SINGLE__)
#undef __PASS_AS_FLOAT
#define __PASS_AS_FLOAT(TYPE_CLASS,SIZE) \
  (TYPE_CLASS == __real_type_class && SIZE <= 8 \
   || TYPE_CLASS == __complex_type_class && SIZE <= 16)
#undef __TARGET_SH4_P
#define __TARGET_SH4_P 1
#endif

#define va_arg(pvar,TYPE)					\
__extension__							\
({int __type = __builtin_classify_type (* (TYPE *) 0);		\
  void * __result_p;						\
  if (__PASS_AS_FLOAT (__type, sizeof(TYPE)))			\
    {								\
      if ((pvar).__va_next_fp < (pvar).__va_next_fp_limit)	\
	{							\
	  if (((__type == __real_type_class && sizeof (TYPE) > 4)\
	       || sizeof (TYPE) > 8)				\
	      && (((int) (pvar).__va_next_fp ^ (int) (pvar).__va_next_fp_limit)\
		  & 4))						\
	    (pvar).__va_next_fp++;				\
	  __result_p = &(pvar).__va_next_fp;			\
	}							\
      else							\
	__result_p = &(pvar).__va_next_stack;			\
    }								\
  else								\
    {								\
      if ((pvar).__va_next_o + ((sizeof (TYPE) + 3) / 4)	\
	  <= (pvar).__va_next_o_limit) 				\
	__result_p = &(pvar).__va_next_o;			\
      else							\
	{							\
	  if (sizeof (TYPE) > 4)				\
	   if (! __TARGET_SH4_P)				\
	    (pvar).__va_next_o = (pvar).__va_next_o_limit;	\
								\
	  __result_p = &(pvar).__va_next_stack;			\
	}							\
    } 								\
  __va_arg_sh1(*(void **)__result_p, TYPE);})

#else /* ! SH3E */

#define va_arg(AP, TYPE) __va_arg_sh1((AP), TYPE)

#endif /* SH3E */

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) ((dest) = (src))

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
