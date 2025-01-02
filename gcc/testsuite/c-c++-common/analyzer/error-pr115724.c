/* Verify that the analyzer handles the no-optimization case in
   glibc's <error.h> when error,error_at_line calls become
   __error_alias and __error_at_line_alias.  */

typedef __SIZE_TYPE__ size_t;
#define EXIT_FAILURE 1
#define __extern_always_inline  extern inline __attribute__ ((__always_inline__)) __attribute__ ((__gnu_inline__))

int errno;

/* Adapted from glibc's bits/error.h.  */

extern void __error_alias (int __status, int __errnum,
			   const char *__format, ...)
  __attribute__ ((__format__ (__printf__, 3, 4)));
extern void __error_noreturn (int __status, int __errnum,
			       const char *__format, ...)
  __attribute__ ((__noreturn__, __format__ (__printf__, 3, 4)));

/* If we know the function will never return make sure the compiler
   realizes that, too.  */
__extern_always_inline void
error (int __status, int __errnum, const char *__format, ...)
{
  if (__builtin_constant_p (__status) && __status != 0)
    __error_noreturn (__status, __errnum, __format, __builtin_va_arg_pack ());
  else
    __error_alias (__status, __errnum, __format, __builtin_va_arg_pack ());
}

extern void __error_at_line_alias (int __status, int __errnum,
				   const char *__fname,
				   unsigned int __line,
				   const char *__format, ...)
  __attribute__ ((__format__ (__printf__, 5, 6)));
extern void __error_at_line_noreturn (int __status, int __errnum,
				      const char *__fname,
				      unsigned int __line,
				      const char *__format,
				      ...)
  __attribute__ ((__noreturn__, __format__ (__printf__, 5, 6)));

/* If we know the function will never return make sure the compiler
   realizes that, too.  */
__extern_always_inline void
error_at_line (int __status, int __errnum, const char *__fname,
	       unsigned int __line, const char *__format, ...)
{
  if (__builtin_constant_p (__status) && __status != 0)
    __error_at_line_noreturn (__status, __errnum, __fname, __line, __format,
			      __builtin_va_arg_pack ());
  else
    __error_at_line_alias (__status, __errnum, __fname, __line,
			   __format, __builtin_va_arg_pack ());
}


struct list {
        size_t size;
        void (*destroy)(void *data);
        struct list_node *head;
        struct list_node *tail;
};

struct list *list_create(void (*destroy)(void *data))
{
        struct list *result = (struct list *)__builtin_calloc(1, sizeof(*result));
        if (!result)
                error(EXIT_FAILURE,errno,"%s:%d %s()",__FILE__,__LINE__,__func__);

        result->destroy = destroy; /* { dg-bogus "dereference of NULL 'result'" } */

        return result;
}

struct list *list_create_using_at_line(void (*destroy)(void *data))
{
        struct list *result = (struct list *)__builtin_calloc(1, sizeof(*result));
        if (!result)
		error_at_line(EXIT_FAILURE,errno,__FILE__,__LINE__,
			      "%s()", __func__);

        result->destroy = destroy; /* { dg-bogus "dereference of NULL 'result'" } */

        return result;
}
