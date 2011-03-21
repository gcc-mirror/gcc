/* PR target/47201 */
/* { dg-do compile } */
/* { dg-options "-O -fpic -g" { target fpic } } */

union U
{
  __UINTPTR_TYPE__ m;
  float d;
} u;

int
foo (void)
{
  union U v = {
    (__UINTPTR_TYPE__)&u
  };
  return u.d == v.d;
}
