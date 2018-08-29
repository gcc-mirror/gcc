/* PR sanitizer/81094 */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size" } */

#define N 20

struct S { int i; };

__attribute__((noinline, noclone)) void
f0 (struct S s)
{
  asm volatile ("" : : "r" (s.i) : "memory");
}

__attribute__((noinline, noclone)) void
f1 (int i)
{
  char *orig;
  struct S *p;
  orig = (char *) __builtin_calloc (N, sizeof (struct S));
  p = (struct S *) orig;
  f0 (*(p + i));
  f0 (p[i]);
  p++;
  f0 (p[i - 1]);
  f0 (*(p + i - 1));
  __builtin_free (orig);
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */

int
main ()
{
  f1 (N);
  return 0;
}
