/* GNU C varargs and stdargs support for the DEC Alpha.  */

/* Note:  We must use the name __builtin_savregs.  GCC attaches special
   significance to that name.  In particular, regardless of where in a
   function __builtin_saveregs is called, GCC moves the call up to the
   very start of the function.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

/* In VMS, __gnuc_va_list is simply char *; on OSF, it's a structure.  */

#ifdef __VMS__
typedef char *__gnuc_va_list;
#else

typedef struct {
  char *__base;			/* Pointer to first integer register. */
  int __offset;			/* Byte offset of args so far. */
} __gnuc_va_list;
#endif

#endif /* __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */

#if !defined(__GNUC_VA_LIST_1) && (defined (_STDARG_H) || defined (_VARARGS_H))
#define __GNUC_VA_LIST_1

#define _VA_LIST
#define _VA_LIST_

typedef __gnuc_va_list va_list;

#if !defined(_STDARG_H)

/* varargs support */
#define va_alist __builtin_va_alist
#define va_dcl	 int __builtin_va_alist;...
#ifdef __VMS__
#define va_start(pvar) ((pvar) = __builtin_saveregs ())
#else
#define va_start(pvar) ((pvar) = * (__gnuc_va_list *) __builtin_saveregs ())
#endif

#else /* STDARG.H */

/* ANSI alternative.  */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that firstarg is correct.  */

#ifdef __VMS__
#define va_start(pvar, firstarg)				\
  (__builtin_next_arg (firstarg),				\
   (pvar) = __builtin_saveregs ())
#else
#define va_start(pvar, firstarg)				\
  (__builtin_next_arg (firstarg),				\
   (pvar) = *(__gnuc_va_list *) __builtin_saveregs ())
#endif

#endif /* _STDARG_H */

#define va_end(__va)	((void) 0)

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

/* Note that parameters are always aligned at least to a word boundary
   (when passed) regardless of what GCC's __alignof__ operator says.  */

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

/* Get the size of a type in bytes, rounded up to an integral number
   of words.  */

#define __va_tsize(__type)  \
  (((sizeof (__type) + __extension__ sizeof (long long) - 1)   \
    / __extension__ sizeof (long long)) * __extension__ sizeof (long long))

#ifdef __VMS__
#define va_arg(__va, __type)						\
(*(((__va) += __va_tsize (__type)),					\
   (__type *)(void *)((__va) - __va_tsize (__type))))

#else

#define va_arg(__va, __type)						\
(*(((__va).__offset += __va_tsize (__type)),				\
   (__type *)(void *)((__va).__base + (__va).__offset			\
	      - (((__builtin_classify_type (* (__type *) 0)		\
		   == __real_type_class) && (__va).__offset <= (6 * 8))	\
		 ? (6 * 8) + 8 : __va_tsize (__type)))))
#endif

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* __GNUC_VA_LIST_1 */
