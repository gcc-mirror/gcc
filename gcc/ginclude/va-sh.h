/* This is just like the default gvarargs.h
   except for differences described below.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

#ifdef __SH3E__

typedef long __va_greg;
typedef double __va_freg;

typedef struct {
  __va_greg * __va_next_o;		/* next available register */
  __va_greg * __va_next_o_limit;	/* past last available register */
  __va_freg * __va_next_fp;		/* next available fp register */
  __va_freg * __va_next_fp_limit;	/* last available fp register */
  __va_greg * __va_next_stack;		/* next extended word on stack */
} __gnuc_va_list;

#else /* ! SH3E */

typedef void *__gnuc_va_list;

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#endif /* ! SH3E */

#endif /* __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H

#ifdef __SH3E__

#define va_start(AP, LASTARG) \
__extension__ \
  ({ \
     AP.__va_next_fp = (__va_freg *) __builtin_saveregs (); \
     AP.__va_next_fp_limit = (AP.__va_next_fp + \
			      (__builtin_args_info (1) < 8 ? 8 - __builtin_args_info (1) : 0)); \
     AP.__va_next_o = (__va_greg *) AP.__va_next_fp_limit; \
     AP.__va_next_o_limit = (AP.__va_next_o + \
			     (__builtin_args_info (0) < 4 ? 4 - __builtin_args_info (0) : 0)); \
     AP.__va_next_stack = (__va_greg *) __builtin_next_arg (LASTARG); \
  })

#else /* ! SH3E */

#define va_start(AP, LASTARG) 						\
 (AP = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))

#endif /* ! SH3E */

#else /* _VARARGS_H */

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist;...

#ifdef __SH3E__

#define va_start(AP) \
__extension__ \
  ({ \
     AP.__va_next_fp = (__va_freg *) __builtin_saveregs (); \
     AP.__va_next_fp_limit = (AP.__va_next_fp + \
			      (__builtin_args_info (1) < 8 ? 8 - __builtin_args_info (1) : 0)); \
     AP.__va_next_o = (__va_greg *) AP.__va_next_fp_limit; \
     AP.__va_next_o_limit = (AP.__va_next_o + \
			     (__builtin_args_info (0) < 4 ? 4 - __builtin_args_info (0) : 0)); \
     AP.__va_next_stack = (__va_greg *) __builtin_next_arg (__builtin_va_alist) \
       - (__builtin_args_info (0) >= 4 || __builtin_args_info (1) >= 8 ? 1 : 0); \
  })

#else /* ! SH3E */

#define va_start(AP)  AP=(char *) &__builtin_va_alist

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

/* RECORD_TYPE args passed using the C calling convention are
   passed by invisible reference.  ??? RECORD_TYPE args passed
   in the stack are made to be word-aligned; for an aggregate that is
   not word-aligned, we advance the pointer to the first non-reg slot.  */

#ifdef __SH3E__

#ifdef __LITTLE_ENDIAN__

#define va_arg(pvar,TYPE)					\
__extension__							\
(*({int __type = __builtin_classify_type (* (TYPE *) 0);	\
  void * __result;						\
  if (__type == __real_type_class && sizeof(TYPE) == 4)		\
						/* float? */	\
    {								\
      __va_freg *__r;						\
      if (pvar.__va_next_fp < pvar.__va_next_fp_limit)		\
	__r = (__va_freg *) pvar.__va_next_fp++;		\
      else							\
	__r = (__va_freg *) pvar.__va_next_stack++;		\
      __result = (char *) __r;					\
    }								\
  else								\
    {								\
      __va_greg *_r;						\
      if (pvar.__va_next_o + ((sizeof (TYPE) + 3) / 4)		\
	  <= pvar.__va_next_o_limit) 				\
	{							\
	  _r = pvar.__va_next_o;				\
	  pvar.__va_next_o += (sizeof (TYPE) + 3) / 4;		\
	}							\
      else							\
	{							\
	  _r = pvar.__va_next_stack;				\
	  pvar.__va_next_stack += (sizeof (TYPE) + 3) / 4;	\
	}							\
      __result = (char *) _r;					\
    } 								\
  (TYPE *) __result;}))

#else /* ! __LITTLE_ENDIAN__ */

#define va_arg(pvar,TYPE)					\
__extension__							\
(*({int __type = __builtin_classify_type (* (TYPE *) 0);	\
  void * __result;						\
  if (__type == __real_type_class && sizeof(TYPE) == 4)		\
						/* float? */	\
    {								\
      __va_freg *__r;						\
      if (pvar.__va_next_fp < pvar.__va_next_fp_limit)		\
	__r = (__va_freg *) pvar.__va_next_fp++;		\
      else							\
	__r = (__va_freg *) pvar.__va_next_stack++;		\
      __result = (char *) __r;					\
    }								\
  else								\
    {								\
      __va_greg *_r;						\
      if (pvar.__va_next_o + ((sizeof (TYPE) + 3) / 4)		\
	  <= pvar.__va_next_o_limit) 				\
	{							\
	  pvar.__va_next_o += (sizeof (TYPE) + 3) / 4;		\
	  _r = pvar.__va_next_o;				\
	}							\
      else							\
	{							\
	  pvar.__va_next_stack += (sizeof (TYPE) + 3) / 4;	\
	  _r = pvar.__va_next_stack;				\
	}							\
      __result = ((char *) _r					\
		  - (sizeof (TYPE) < 4 ? sizeof (TYPE)		\
		     : ((sizeof (TYPE) + 3) / 4) * 4));		\
    } 								\
  (TYPE *) __result;}))

#endif /* __LITTLE_ENDIAN__ */

#else /* ! SH3E */

#ifdef __LITTLE_ENDIAN__

/* This is for little-endian machines; small args are padded upward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
  *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (TYPE))))

#else /* ! __LITTLE_ENDIAN__ */

/* This is for big-endian machines; small args are padded downward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
  *((TYPE *) (void *) ((char *) (AP)					\
		       - ((sizeof (TYPE) < __va_rounded_size (char)	\
			   ? sizeof (TYPE) : __va_rounded_size (TYPE))))))

#endif /* __LITTLE_ENDIAN__ */

#endif /* SH3E */

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
