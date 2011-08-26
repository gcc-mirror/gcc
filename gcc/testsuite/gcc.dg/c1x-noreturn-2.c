/* Test C1X _Noreturn.  Test valid code using stdnoreturn.h.  */
/* { dg-do run } */
/* { dg-options "-std=c1x -pedantic-errors" } */

#include <stdnoreturn.h>

extern int strcmp (const char *, const char *);

noreturn void exit (int);
noreturn void abort (void);

noreturn int f1 (void);

noreturn void f2 (void);

static void noreturn f3 (void) { exit (0); }

/* Returning from a noreturn function is undefined at runtime, not a
   constraint violation, but recommended practice is to diagnose if
   such a return appears possible.  */

noreturn int
f4 (void)
{
  return 1; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } 25 } */
}

noreturn void
f5 (void)
{
  return; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } 32 } */
}

noreturn void
f6 (void)
{
} /* { dg-warning "does return" } */

noreturn void
f7 (int a)
{
  if (a)
    exit (0);
} /* { dg-warning "does return" } */

/* Declarations need not all have noreturn.  */

void f2 (void);

void f8 (void);
noreturn void f8 (void);

/* Duplicate noreturn is OK.  */
noreturn noreturn void noreturn f9 (void);

/* noreturn does not affect type compatibility.  */

void (*fp) (void) = f5;

#ifndef noreturn
#error "noreturn not defined"
#endif

#define str(x) #x
#define xstr(x) str(x)

const char *s = xstr(noreturn);

int
main (void)
{
  if (strcmp (s, "_Noreturn") != 0)
    abort ();
  exit (0);
}
