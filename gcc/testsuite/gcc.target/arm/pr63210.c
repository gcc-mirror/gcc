/* { dg-do assemble } */
/* { dg-options "-Os" }  */
/* { dg-require-effective-target arm_arch_v5t_thumb_ok } */
/* { dg-add-options arm_arch_v5t_thumb } */

int foo1 (int c);
int foo2 (int c);

int test (int c)
{
  return (foo1 (c) || foo2 (c));
}
/* { dg-final { object-size text <= 28 } } */
