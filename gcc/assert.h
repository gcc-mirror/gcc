/* Allow this file to be included multiple times
   with different settings of NDEBUG.  */
#undef assert
#undef __assert

#ifdef NDEBUG
#define assert(ignore) ((void) 0)
#else

#ifndef __GNUC__

#define assert(expression)  \
  ((void) ((expression) ? 0 : __assert (expression, __FILE__, __LINE__)))

#define __assert(expression, file, lineno)  \
  (printf ("%s:%d: failed assertion\n", file, lineno),	\
   abort (), 0)

#else

#ifdef __STDC__

/* Defined in libgcc.a */
extern void __eprintf (const char *, const char *, int, const char *);

#define assert(expression)  \
  ((void) ((expression) ? 0 : __assert (#expression, __FILE__, __LINE__)))

#define __assert(expression, file, line)  \
  (__eprintf ("%s:%d: failed assertion `%s'\n",		\
	      file, line, expression), 0)

#else /* no __STDC__; i.e. -traditional.  */

extern void __eprintf (); /* Defined in libgcc.a */

#define assert(expression)  \
  ((void) ((expression) ? 0 : __assert (expression, __FILE__, __LINE__)))

#define __assert(expression, file, lineno)  \
  (__eprintf ("%s:%d: failed assertion `%s'\n",		\
	      file, lineno, "expression"), 0)

#endif /* no __STDC__; i.e. -traditional.  */
#endif /* no __GNU__; i.e., /bin/cc.  */
#endif
