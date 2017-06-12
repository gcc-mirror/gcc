/* Test C11 _Noreturn.  Test valid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Noreturn void exit (int);

_Noreturn int f1 (void);

_Noreturn void f2 (void);

static void _Noreturn f3 (void) { exit (0); }

/* Returning from a noreturn function is undefined at runtime, not a
   constraint violation, but recommended practice is to diagnose if
   such a return appears possible.  */

_Noreturn int
f4 (void)
{
  return 1; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } .-1 } */
}

_Noreturn void
f5 (void)
{
  return; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } .-1 } */
}

_Noreturn void
f6 (void)
{
} /* { dg-warning "does return" } */

_Noreturn void
f7 (int a)
{
  if (a)
    exit (0);
} /* { dg-warning "does return" } */

/* Declarations need not all have _Noreturn.  */

void f2 (void);

void f8 (void);
_Noreturn void f8 (void);

/* Duplicate _Noreturn is OK.  */
_Noreturn _Noreturn void _Noreturn f9 (void);

/* _Noreturn does not affect type compatibility.  */

void (*fp) (void) = f5;

/* noreturn is an ordinary identifier.  */

int noreturn;
