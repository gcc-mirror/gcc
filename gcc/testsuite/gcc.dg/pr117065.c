/* PR middle-end/117065 */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

union U { struct A a; unsigned long long b; };	/* { dg-error "field 'a' has incomplete type" } */

union U
foo (void)
{
  union U u = { .b = 1 };
  return u;
}
