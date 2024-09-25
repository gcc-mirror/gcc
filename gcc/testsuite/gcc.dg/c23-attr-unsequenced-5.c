/* Test C23 unsequenced attribute: composite type on ?:.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-additional-sources "c23-attr-unsequenced-5-aux.c" } */

int f1 () [[unsequenced]];
int f2 ();
int f3 ();
int (*fp1) () [[unsequenced]] = f2;
int (*fp2) () [[unsequenced]] = f3;
extern void abort ();

int
foo (int x)
{
  return __builtin_has_attribute (*(x ? f1 : f3), unsequenced);
}

int
bar (int x)
{
  return __builtin_has_attribute (*(x ? fp1 : fp2), unsequenced);
}

int
baz (int x)
{
  return __builtin_has_attribute (*(x ? f3 : f1), unsequenced);
}

int
qux (int x)
{
  return __builtin_has_attribute (*(x ? fp2 : fp1), unsequenced);
}

int
main ()
{
  if (!foo (0) || !bar (0) || !baz (0) || !qux (0))
    abort ();
  if (!foo (1) || !bar (1) || !baz (1) || !qux (1))
    abort ();
}
