/* Test C23 noreturn attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

[[noreturn]] void exit (int);

[[__noreturn__]] int f1 (void);

[[_Noreturn]] void f2 (void);

[[___Noreturn__]] static void f3 (void) { exit (0); }

/* Returning from a noreturn function is undefined at runtime, not a
   constraint violation, but recommended practice is to diagnose if
   such a return appears possible.  */

[[noreturn]] int
f4 (void)
{
  return 1; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } .-1 } */
}

[[__noreturn__]] void
f5 (void)
{
  return; /* { dg-warning "has a 'return' statement" } */
  /* { dg-warning "does return" "second warning" { target *-*-* } .-1 } */
}

[[_Noreturn]] void
f6 (void)
{
} /* { dg-warning "does return" } */

[[___Noreturn__]] void
f7 (int a)
{
  if (a)
    exit (0);
} /* { dg-warning "does return" } */

/* Declarations need not all have the attribute (buf if the first does not,
   there is undefined behavior).  */

void f2 (void);

/* Duplicate attribute, and use with _Noreturn, is OK.  */
[[noreturn]] [[noreturn]] [[noreturn, __noreturn__]] void _Noreturn f9 (void);

/* The attribute does not affect type compatibility.  */

void (*fp) (void) = f5;

/* Unlike the function specifier, the attribute may be used on main.  */
[[noreturn]] int main ();
