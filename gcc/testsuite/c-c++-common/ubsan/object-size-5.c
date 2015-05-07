/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size" } */

/* Test structures with -fsanitize=object-size.  */

#define N 20

struct S { char *p; int i; };
struct T { struct S *s; };

__attribute__((noinline, noclone)) void
f1 (int i)
{
  volatile int j;
  struct S s;
  s.p = (char *) __builtin_calloc (N, 1);
  j = s.p[i];
  j = *(s.p + i);
  __builtin_free (s.p);
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */

int
main ()
{
  f1 (N);
  f1 (N - 1);
  return 0;
}
