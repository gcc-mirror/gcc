/* PR rtl-optimization/58668 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mthumb" { target { { arm*-*-* } && arm_thumb2_ok } } } */

void *fn1 (void *);
void *fn2 (void *, const char *);
void fn3 (void *);
void fn4 (void *, int);

void *
test (void *x)
{
  void *a, *b;
  if (!(a = fn1 (x)))
    return (void *) 0;
  if (!(b = fn2 (a, "w")))
    {
      fn3 (a);
      return (void *) 0;
    }
  fn3 (a);
  fn4 (b, 1);
  return b;
}
