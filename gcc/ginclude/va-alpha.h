/* GNU C varargs and stdargs support for the DEC Alpha.  */

/* Note:  We must use the name __builtin_savregs.  GCC attaches special
   significance to that name.  In particular, regardless of where in a
   function __builtin_saveregs is called, GCC moves the call up to the
   very start of the function.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct {
  long __va_arg;		/* Current argument number. */
  long *__va_stack;		/* Start of arguments on stack */
  long *__va_iregs;		/* Integer parameter registers ($16-$21) */
  long *__va_fregs;		/* FP parameter registers ($f16-$f21) */
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#define va_list __gnuc_va_list
#define _VA_LIST
#define _VA_LIST_

#if !defined(_STDARG_H)

/* varargs support */
#define va_alist __builtin_va_alist
#define va_dcl
#define va_start(pvar) ((pvar) = * (__gnuc_va_list *) __builtin_saveregs ())

#else /* STDARG.H */

/* ANSI alternative.  */

#define va_start(pvar, firstarg)  \
  ((pvar) = *(__gnuc_va_list *) __builtin_saveregs ())

#endif /* _STDARG_H */

#ifndef va_end

#define va_end(__va)

/* Values returned by __builtin_classify_type.  */

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

/* Note that parameters are always aligned at least to a word boundary
   (when passed) regardless of what GCC's __alignof__ operator says.  */

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

/* Get the rounded number of words of a type.  */

#define __va_nwords(__type)  \
  ((sizeof (__type) + sizeof (long) - 1) / sizeof (long))

#define va_arg(__va, __type)						\
__extension__								\
(* (__type *)								\
 ({									\
  register void *__rv;  /* result value */				\
  switch (__builtin_classify_type (* (__type *) 0))			\
    {			        					\
    case __real_type_class:						\
									\
      /* Get a pointer to the value.  If we want a float instead of	\
	 a double, we have to make one and point to it instead.  */     \
									\
      __rv = (void *) & ((__va).__va_arg < 6				\
			 ? (__va).__va_fregs[(__va).__va_arg]		\
			 : (__va).__va_stack[(__va).__va_arg - 6]);	\
									\
      if (sizeof (__type) == sizeof (float))				\
	{								\
	  float __rf = * ((double *) __rv);				\
									\
	  __rv = (void *) &__rf;					\
	}								\
									\
      break;								\
	      								\
    case __void_type_class:						\
    case __integer_type_class:						\
    case __char_type_class:						\
    case __enumeral_type_class:						\
    case __boolean_type_class:						\
    case __pointer_type_class:						\
    case __reference_type_class:					\
    case __offset_type_class:						\
    case __record_type_class:						\
    case __union_type_class:						\
									\
      /* Force this on the stack if it's alignment isn't right.  */	\
									\
      if ((__va).__va_arg < 6)						\
	switch (sizeof (__type))					\
	  {								\
	  case sizeof (char):						\
	    break;							\
	  case sizeof (short):						\
	    if (__alignof__ (__type) < sizeof (short))			\
	      (__va).__va_arg = 6;					\
	    break;							\
	  case 3:							\
	  case sizeof (int):						\
	    if (__alignof__ (__type) < sizeof (int))			\
	      (__va).__va_arg = 6;					\
	    break;							\
	  default:							\
	    if (__alignof__ (__type) < sizeof (long))			\
	      (__va).__va_arg = 6;					\
	    break;							\
	  }								\
									\
      /* If this object is only one word long, just get it.  If it is   \
	 longer, we need to worry about the possibility that it is	\
	 passed both in registers and in memory.  */			\
									\
      if (sizeof (__type) <= sizeof (long)				\
	  || (__va).__va_arg >= 6					\
	  || (__va).__va_arg + __va_nwords (__type) < 6)		\
	__rv = (void *) & ((__va).__va_arg < 6				\
			   ? (__va).__va_iregs[(__va).__va_arg]		\
			   : (__va).__va_stack[(__va).__va_arg - 6]);	\
      else								\
	{								\
	  long __obj[__va_nwords (__type)];				\
	  int __i;							\
									\
	  for (__i = 0; __i < __va_nwords (__type); __i++)		\
	    __obj[__i] = ((__va).__va_arg + __i < 6			\
			  ? (__va).__va_iregs[(__va).__va_arg + __i]	\
			  : (__va).__va_stack[(__va).__va_arg + __i - 6]); \
									\
	  __rv = (void *) &__obj[0];					\
	}								\
      break;								\
									\
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
									\
  (__va).__va_arg += __va_nwords (__type);				\
									\
  __rv;									\
}))

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

