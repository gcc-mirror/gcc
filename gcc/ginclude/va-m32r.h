/* GNU C stdarg/varargs support for the M32R */

/* Define __gnuc_va_list.  */
#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef void *__gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

/* Common code for va_start for both varargs and stdarg.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#ifdef _STDARG_H /* stdarg.h support */

/* Calling __builtin_next_arg gives the proper error message if LASTARG is
   not indeed the last argument.  */
#define va_start(AP, LASTARG) 						\
 (AP = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))

#else /* varargs.h support */

#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; ...
#define va_start(AP)  AP=(char *) &__builtin_va_alist

#endif /* _STDARG_H */

/* Nothing needs to be done to end varargs/stdarg processing */
#define va_end(AP) ((void) 0)

/* Values returned by __builtin_classify_type.  */
enum __type_class
{
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

/* Return whether a type is passed by reference.  */
#define __va_by_reference_p(TYPE) (sizeof (TYPE) > 8)

#define va_arg(AP,TYPE)							\
__extension__ (*({							\
  register TYPE *__ptr;							\
									\
  if (__va_by_reference_p (TYPE))					\
    {									\
      __ptr = *(TYPE **)(void *) (AP);					\
      (AP) = (__gnuc_va_list) ((char *) (AP) + sizeof (void *));	\
    }									\
  else									\
    {									\
      __ptr = (TYPE *)(void *)						\
        ((char *) (AP) + (sizeof (TYPE) < __va_rounded_size (char)	\
			  ? __va_rounded_size (TYPE) - sizeof (TYPE)	\
			  : 0));					\
      (AP) = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)); \
    }									\
									\
  __ptr;								\
}))

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
