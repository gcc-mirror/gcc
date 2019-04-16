/* PR middle-end/81824 - Warn for missing attributes with function aliases
   Verify that attributes always_inline, gnu_inline, and noinline aren't
   copied.  Also verify that copying attribute tls_model to a non-thread
   variable triggers a warning.
   { dg-do compile }
   { dg-require-alias "" }
   { dg-options "-Wall" }
   { dg-require-effective-target tls } */

#define ATTR(...)   __attribute__ ((__VA_ARGS__))

ATTR (always_inline, gnu_inline, noreturn) inline int
finline_noret (void)
{
  __builtin_abort ();
  /* Expect no -Wreturn-type.  */
}

int call_finline_noret (void)
{
  finline_noret ();
  /* Expect no -Wreturn-type.  */
}


ATTR (copy (finline_noret)) int
fnoret (void);

int call_fnoret (void)
{
  fnoret ();
  /* Expect no -Wreturn-type.  */
}


/* Verify that attribute always_inline on an alias target doesn't
   get copied and interfere with attribute noinline on the alias
   (trigger a warning due to a conflict).  */

ATTR (always_inline) static inline int
finline (void) { return 0; }

ATTR (alias ("finline"), noinline) int
fnoinline (void);

ATTR (copy (finline)) int
fnoinline (void);


ATTR (tls_model ("global-dynamic")) __thread int
  tls_target;

ATTR (alias ("tls_target"), copy (tls_target)) extern __thread int
  thread_alias;


ATTR (alias ("tls_target"), copy (tls_target)) extern int
  alias;            /* { dg-warning ".tls_model. attribute ignored because .alias. does not have thread storage duration" } */
