/* This is just like the default gvarargs.h
   except for differences described below.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

#ifndef __svr4__
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

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#ifdef _STDARG_H
#define va_start(AP, LASTARG)					\
  (__builtin_saveregs (), AP = ((char *) __builtin_next_arg ()))
#else
#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; __va_ellipsis

#define va_start(AP) 						\
 (__builtin_saveregs (), (AP) = ((char *) &__builtin_va_alist))
#endif

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
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
/* We don't declare the union member `d' to have type TYPE
   because that would lose in C++ if TYPE has a constructor.  */
/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
#define va_arg(pvar,TYPE)					\
__extension__							\
({ TYPE __va_temp;						\
   ((__builtin_classify_type (__va_temp) >= 12)			\
    ? ((pvar) += __va_rounded_size (TYPE *),			\
       **(TYPE **) (void *) ((pvar) - __va_rounded_size (TYPE *))) \
    : __va_rounded_size (TYPE) == 8				\
    ? ({ union {char __d[sizeof (TYPE)]; int __i[2];} __u;	\
	 __u.__i[0] = ((int *) (void *) (pvar))[0];		\
	 __u.__i[1] = ((int *) (void *) (pvar))[1];		\
	 (pvar) += 8;						\
	 *(TYPE *) (void *) __u.__d; })				\
    : ((pvar) += __va_rounded_size (TYPE),			\
       *((TYPE *) (void *) ((pvar) - __va_rounded_size (TYPE)))));})

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

