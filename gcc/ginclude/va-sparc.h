/* This is just like the default gvarargs.h
   except for differences described below.  */

/* Make this a macro rather than a typedef, so we can undef any other defn.  */
#define va_list __va___list
/* This has to be a char * to be compatible with Sun.
   i.e., we have to pass a `va_list' to vsprintf.  */
typedef char * __va___list;

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; __va_ellipsis

#ifdef _STDARG_H
#define va_start(AP, LASTARG)					\
  (__builtin_saveregs (), AP = ((char *) __builtin_next_arg ()))
#else
#define va_start(AP) 						\
 (__builtin_saveregs (), (AP) = ((char *) &__builtin_va_alist))
#endif

#define va_end(pvar)

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

/* RECORD_TYPE args passed using the C calling convention are
   passed by invisible reference.  ??? RECORD_TYPE args passed
   in the stack are made to be word-aligned; for an aggregate that is
   not word-aligned, we advance the pointer to the first non-reg slot.  */
#define va_arg(pvar,TYPE)					\
__extension__							\
({ TYPE __va_temp;						\
   ((__builtin_classify_type (__va_temp) >= 12)			\
    ? ((pvar) += __va_rounded_size (TYPE *),			\
       **(TYPE **) ((pvar) - __va_rounded_size (TYPE *)))	\
    : ((pvar) += __va_rounded_size (TYPE),			\
       *((TYPE *) ((pvar) - __va_rounded_size (TYPE)))));})
