/* GNU C varargs and stdargs support for the DEC Alpha.  */

/* Note:  We must use the name __builtin_savregs.  GCC attaches special
   significance to that name.  In particular, regardless of where in a
   function __builtin_saveregs is called, GCC moves the call up to the
   very start of the function.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct {
  char *__base;			/* Pointer to first integer register. */
  long __offset;		/* Byte offset of args so far. */
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
#define va_dcl	 int __builtin_va_alist;...
#define va_start(pvar) ((pvar) = * (__gnuc_va_list *) __builtin_saveregs ())

#else /* STDARG.H */

/* ANSI alternative.  */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that firstarg is correct.  */
#define va_start(pvar, firstarg)				\
  (__builtin_next_arg (firstarg),				\
   (pvar) = *(__gnuc_va_list *) __builtin_saveregs ())

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

/* Get the size of a type in bytes, rounded up to an integral number
   of words.  */

#define __va_tsize(__type)  \
  (((sizeof (__type) + sizeof (long) - 1) / sizeof (long)) * sizeof (long))

#define va_arg(__va, __type)						\
(*(((__va).__offset += __va_tsize (__type)),				\
   (__type *)(void *)((__va).__base + (__va).__offset			\
	      - (((__builtin_classify_type (* (__type *) 0)		\
		   == __real_type_class) && (__va).__offset <= (6 * 8))	\
		 ? (6 * 8) + 8 : __va_tsize (__type)))))

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

