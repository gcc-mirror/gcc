/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

/* Check that impermissible (cross-strub-context) calls are reported.  */

extern int __attribute__ ((__strub__ ("callable"))) xcallable (void);
extern int __attribute__ ((__strub__ ("internal"))) xinternal (void);
extern int __attribute__ ((__strub__ ("at-calls"))) xat_calls (void);
extern int __attribute__ ((__strub__ ("disabled"))) xdisabled (void);

int __attribute__ ((__strub__ ("callable"))) callable (void);
int __attribute__ ((__strub__ ("internal"))) internal (void);
int __attribute__ ((__strub__ ("at-calls"))) at_calls (void);
int __attribute__ ((__strub__ ("disabled"))) disabled (void);

int __attribute__ ((__strub__)) var;
int var_user (void);

static inline int __attribute__ ((__always_inline__, __strub__ ("callable")))
icallable (void);
static inline int __attribute__ ((__always_inline__, __strub__ ("internal")))
iinternal (void);
static inline int __attribute__ ((__always_inline__, __strub__ ("at-calls")))
iat_calls (void);
static inline int __attribute__ ((__always_inline__, __strub__ ("disabled")))
idisabled (void);
static inline int __attribute__ ((__always_inline__))
ivar_user (void);

static inline int __attribute__ ((__always_inline__, __strub__ ("callable")))
i_callable (void) { return 0; }
static inline int __attribute__ ((__always_inline__, __strub__ ("internal")))
i_internal (void) { return var; }
static inline int __attribute__ ((__always_inline__, __strub__ ("at-calls")))
i_at_calls (void) { return var; }
static inline int __attribute__ ((__always_inline__, __strub__ ("disabled")))
i_disabled (void) { return 0; }
static inline int __attribute__ ((__always_inline__))
i_var_user (void) { return var; }

#define CALLS_GOOD_FOR_STRUB_CONTEXT(ISEP)	\
  do {						\
    ret += i ## ISEP ## at_calls ();		\
    ret += i ## ISEP ## internal ();		\
    ret += i ## ISEP ## var_user ();		\
  } while (0)

#define CALLS_GOOD_FOR_NONSTRUB_CONTEXT(ISEP)	\
  do {						\
    ret += internal ();				\
    ret += disabled ();				\
    ret += var_user ();				\
						\
    ret += i ## ISEP ## disabled ();		\
						\
    ret += xinternal ();			\
    ret += xdisabled ();			\
  } while (0)

#define CALLS_GOOD_FOR_EITHER_CONTEXT(ISEP)	\
  do {						\
    ret += i ## ISEP ## callable ();		\
						\
    ret += callable ();				\
    ret += at_calls ();				\
						\
    ret += xat_calls ();			\
    ret += xcallable ();			\
  } while (0)

/* Not a strub context, so it can call anything.
   Explicitly declared as callable even from within strub contexts.  */
int __attribute__ ((__strub__ ("callable")))
callable (void) {
  int ret = 0;

  /* CALLS_GOOD_FOR_STRUB_CONTEXT(); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += iat_calls (); /* { dg-error "in non-.strub. context" } */
    ret += iinternal (); /* { dg-error "in non-.strub. context" } */
    ret += ivar_user (); /* { dg-error "in non-.strub. context" } */
#endif
  CALLS_GOOD_FOR_EITHER_CONTEXT();
  CALLS_GOOD_FOR_NONSTRUB_CONTEXT();

  return ret;
}

/* Internal strubbing means the body is a strub context, so it can only call
   strub functions, and it's not itself callable from strub functions.  */
int __attribute__ ((__strub__ ("internal")))
internal (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT();
  CALLS_GOOD_FOR_EITHER_CONTEXT();
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += idisabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}

int __attribute__ ((__strub__ ("at-calls")))
at_calls (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT();
  CALLS_GOOD_FOR_EITHER_CONTEXT();
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += idisabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}

int __attribute__ ((__strub__ ("disabled")))
disabled () {
  int ret = 0;

  /* CALLS_GOOD_FOR_STRUB_CONTEXT(); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += iat_calls (); /* { dg-error "in non-.strub. context" } */
    ret += iinternal (); /* { dg-error "in non-.strub. context" } */
    ret += ivar_user (); /* { dg-error "in non-.strub. context" } */
#endif
  CALLS_GOOD_FOR_EITHER_CONTEXT();
  CALLS_GOOD_FOR_NONSTRUB_CONTEXT();

  return ret;
}  

int
var_user (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT();
  CALLS_GOOD_FOR_EITHER_CONTEXT();
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += idisabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}

int
icallable (void)
{
  int ret = 0;

  /* CALLS_GOOD_FOR_STRUB_CONTEXT(_); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += i_at_calls (); /* { dg-error "in non-.strub. context" } */
    ret += i_internal (); /* { dg-error "in non-.strub. context" } */
    ret += i_var_user (); /* { dg-error "in non-.strub. context" } */
#endif
  CALLS_GOOD_FOR_EITHER_CONTEXT(_);
  CALLS_GOOD_FOR_NONSTRUB_CONTEXT(_);

  return ret;
}

int
iinternal (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT(_);
  CALLS_GOOD_FOR_EITHER_CONTEXT(_);
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(_); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += i_disabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}

int __attribute__ ((__always_inline__, __strub__ ("at-calls")))
iat_calls (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT(_);
  CALLS_GOOD_FOR_EITHER_CONTEXT(_);
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(_); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += i_disabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}

int
idisabled () {
  int ret = 0;

  /* CALLS_GOOD_FOR_STRUB_CONTEXT(_); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += i_at_calls (); /* { dg-error "in non-.strub. context" } */
    ret += i_internal (); /* { dg-error "in non-.strub. context" } */
    ret += i_var_user (); /* { dg-error "in non-.strub. context" } */
#endif
  CALLS_GOOD_FOR_EITHER_CONTEXT(_);
  CALLS_GOOD_FOR_NONSTRUB_CONTEXT(_);

  return ret;
}  

int
ivar_user (void) {
  int ret = var;

  CALLS_GOOD_FOR_STRUB_CONTEXT(_);
  CALLS_GOOD_FOR_EITHER_CONTEXT(_);
  /* CALLS_GOOD_FOR_NONSTRUB_CONTEXT(_); */
#if !OMIT_IMPERMISSIBLE_CALLS
    ret += internal (); /* { dg-error "in .strub. context" } */
    ret += disabled (); /* { dg-error "in .strub. context" } */
    ret += var_user (); /* { dg-error "in .strub. context" } */

    ret += i_disabled (); /* { dg-error "in .strub. context" } */

    ret += xinternal (); /* { dg-error "in .strub. context" } */
    ret += xdisabled (); /* { dg-error "in .strub. context" } */
#endif

  return ret;
}
