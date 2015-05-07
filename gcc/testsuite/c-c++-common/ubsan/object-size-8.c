/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

struct S { int a; int b; };

static inline __attribute__((always_inline)) int
foo (struct S *p)
{
  volatile int a;
  a = p->a; /* OK */
  return p->b;
}

int
bar (void)
{
  struct S *p = (struct S *) __builtin_calloc (sizeof (int) + sizeof (int) / 2, 1);
  return foo (p);
}

int
main (void)
{
  bar ();
  return 0;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */
