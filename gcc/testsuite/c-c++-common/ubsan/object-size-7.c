/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size" } */

#define N 20

struct S { int a; };

__attribute__((noinline, noclone)) struct S
f1 (int i)
{
  struct S a[N];
  struct S *p = a;
  struct S s;
  s = p[i];
  return s;
}

int
main ()
{
  f1 (N);
  return 0;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */
