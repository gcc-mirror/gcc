/* stdarg/varargs support for the ARC */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef void * __gnuc_va_list;
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

/* See arc_setup_incoming_varargs for reasons for the oddity in va_start.  */
#ifdef _STDARG_H
#define va_start(AP, LASTARG) \
(AP = (__gnuc_va_list) ((int *) __builtin_next_arg (LASTARG) \
			+ (__builtin_args_info (0) < 8 \
			   ? (__builtin_args_info (0) & 1) \
			   : 0)))
#else
#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist; __va_ellipsis
#define va_start(AP) \
(AP = (__gnuc_va_list) ((int *) &__builtin_va_alist \
			+ (__builtin_args_info (0) < 8 \
			   ? (__builtin_args_info (0) & 1) \
			   : 0)))
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
#define va_end(AP)	((void)0)

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

/* All aggregates are passed by reference.  All scalar types larger than 8
   bytes are passed by reference.  */
/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.
   The casts to char * avoid warnings about invalid pointer arithmetic.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#ifdef __big_endian__
#define va_arg(AP,TYPE) \
__extension__							\
(*({((__builtin_classify_type (*(TYPE*) 0) >= __record_type_class \
      || __va_rounded_size (TYPE) > 8)				\
     ? ((AP) = (char *)(AP) + __va_rounded_size (TYPE *),	\
	*(TYPE **) (void *) ((char *)(AP) - __va_rounded_size (TYPE *))) \
     : ((TYPE *) (void *)					\
	(AP = (void *) ((__alignof__ (TYPE) > 4			\
			 ? ((int) AP + 8 - 1) & -8		\
			 : (int) AP)				\
			 + __va_rounded_size (TYPE))) - 1));}))
#else
#define va_arg(AP,TYPE) \
__extension__							\
(*({((__builtin_classify_type (*(TYPE*) 0) >= __record_type_class \
      || __va_rounded_size (TYPE) > 8)				\
     ? ((AP) = (char *)(AP) + __va_rounded_size (TYPE *),	\
	*(TYPE **) (void *) ((char *)(AP) - __va_rounded_size (TYPE *))) \
     : ((AP = (void *) ((__alignof__ (TYPE) > 4			\
			? ((int) AP + 8 - 1) & -8		\
			: (int) AP)				\
		       + __va_rounded_size (TYPE))),		\
	(TYPE *) (void *) (AP - __va_rounded_size (TYPE))));}))
#endif

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
