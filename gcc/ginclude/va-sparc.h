/* This is just like the default gvarargs.h
   except for differences described below.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
#if ! defined (__svr4__) && ! defined (__linux__) && ! defined (__arch64__)
/* This has to be a char * to be compatible with Sun.
   i.e., we have to pass a `va_list' to vsprintf.  */
typedef char * __gnuc_va_list;
#else
/* This has to be a void * to be compatible with Sun svr4.
   i.e., we have to pass a `va_list' to vsprintf.  */
typedef void * __gnuc_va_list;
#endif
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that LASTARG is correct.  */
#if defined (__GCC_NEW_VARARGS__) || defined (__arch64__)
#define va_start(AP, LASTARG) \
  (__builtin_next_arg (LASTARG), AP = (char *) __builtin_saveregs ())
#else
#define va_start(AP, LASTARG)					\
  (__builtin_saveregs (), AP = ((char *) __builtin_next_arg (LASTARG)))
#endif

#else

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist;...

#if defined (__GCC_NEW_VARARGS__) || defined (__arch64__)
#define va_start(AP)	((AP) = (char *) __builtin_saveregs ())
#else
#define va_start(AP) \
  (__builtin_saveregs (), (AP) = ((char *) &__builtin_va_alist))
#endif

#endif

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

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

/* RECORD_TYPE args passed using the C calling convention are
   passed by invisible reference.  ??? RECORD_TYPE args passed
   in the stack are made to be word-aligned; for an aggregate that is
   not word-aligned, we advance the pointer to the first non-reg slot.  */

#ifdef __arch64__

typedef unsigned int __ptrint __attribute__ ((__mode__ (__DI__)));

/* ??? TODO: little endian support */

#define va_arg(pvar, TYPE) \
__extension__							\
(*({int __type = __builtin_classify_type (* (TYPE *) 0);	\
  char * __result;						\
  if (__type == __real_type_class)		/* float? */	\
    {								\
      if (__alignof__ (TYPE) == 16)				\
	(pvar) = (void *) (((__ptrint) (pvar) + 15) & -16);	\
      __result = (pvar);					\
      (pvar) = (char *) (pvar) + sizeof (TYPE);			\
    }								\
  else if (__type < __record_type_class)	/* integer? */	\
    {								\
      (pvar) = (char *) (pvar) + 8;				\
      __result = (char *) (pvar) - sizeof (TYPE);		\
    }								\
  else /* aggregate object */					\
    {								\
      if (sizeof (TYPE) <= 8)					\
	{							\
	  __result = (pvar);					\
	  (pvar) = (char *) (pvar) + 8;				\
	}							\
      else if (sizeof (TYPE) <= 16)				\
	{							\
	  if (__alignof__ (TYPE) == 16)				\
	    (pvar) = (void *) (((__ptrint) (pvar) + 15) & -16);	\
	  __result = (pvar);					\
	  (pvar) = (char *) (pvar) + 16;			\
	}							\
      else							\
	{							\
	  __result = * (void **) (pvar);			\
	  (pvar) = (char *) (pvar) + 8;				\
	}							\
    }								\
  (TYPE *) __result;}))

#else /* not __arch64__ */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

/* We don't declare the union member `d' to have type TYPE
   because that would lose in C++ if TYPE has a constructor.  */
/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.
   The casts to char * avoid warnings about invalid pointer arithmetic.  */
#define va_arg(pvar,TYPE)					\
__extension__							\
(*({((__builtin_classify_type (*(TYPE*) 0) >= __record_type_class \
      || (__builtin_classify_type (*(TYPE*) 0) == __real_type_class \
	  && sizeof (TYPE) == 16))				\
    ? ((pvar) = (char *)(pvar) + __va_rounded_size (TYPE *),	\
       *(TYPE **) (void *) ((char *)(pvar) - __va_rounded_size (TYPE *))) \
    : __va_rounded_size (TYPE) == 8				\
    ? ({ union {char __d[sizeof (TYPE)]; int __i[2];} __u;	\
	 __u.__i[0] = ((int *) (void *) (pvar))[0];		\
	 __u.__i[1] = ((int *) (void *) (pvar))[1];		\
	 (pvar) = (char *)(pvar) + 8;				\
	 (TYPE *) (void *) __u.__d; })				\
    : ((pvar) = (char *)(pvar) + __va_rounded_size (TYPE),	\
       ((TYPE *) (void *) ((char *)(pvar) - __va_rounded_size (TYPE)))));}))

#endif /* not __arch64__ */

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
