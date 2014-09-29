/* { dg-do assemble } */
/* { dg-options "-mthumb -Os " }  */
/* { dg-require-effective-target arm_thumb1_ok } */

int foo1 (int c);
int foo2 (int c);

int test (int c)
{
  return (foo1 (c) || foo2 (c));
}
/* { dg-final { object-size text <= 28 } } */
