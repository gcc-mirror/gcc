#ifndef __rtltypes_h__
#define __rtltypes_h__

#include <setjmp.h>

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

/* argc, argv */
typedef struct
{
    unsigned short	len;
    char		body[0];
} TVaryingCharType;

#ifndef __CHILL_LIB__
extern TVaryingCharType	**chill_argv;
extern int		chill_argc;
#endif

/* definitions for exceptions */
typedef struct
{
    char	*exname;
    short	exnumber;
} TExceptionDefinition;

#if 1
typedef char *__ch_exception;
#define EX_EQ(e1, e2) (strcmp(e1, e2)==0)
#else
typedef void *__ch_exception;
#define EX_EQ(e1, e2) (e1 == e2)
#endif
#define __ch_else_except ((__ch_exception)0)

struct __ch_handled_excepts
{
  /* List is ended by a code==0, or ex==__ch_else_except (ELSE handler). */
  __ch_exception ex;
  int code; /* Positive number indicating ordinal in handler list. */
};

/* definitions for exception handlers */
typedef struct  __ch_handler
{
  struct __ch_handler *prev;
  struct __ch_handled_excepts *handlers;
  jmp_buf jbuf;
} TExceptionHandlerStack;

/* exceptions */
#define EXCEPTION(x)	/* nothing */

#endif /* __rtltypes_h__ */
