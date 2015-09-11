/* { dg-do assemble } */
/* { dg-options "-mthumb -Os " }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-skip-if "do not test on armv4t" { *-*-* } { "-march=armv4t" } } */
/* { dg-additional-options "-march=armv5t" {target arm_arch_v5t_ok} } */

int foo1 (int c);
int foo2 (int c);

int test (int c)
{
  return (foo1 (c) || foo2 (c));
}
/* { dg-final { object-size text <= 28 } } */
