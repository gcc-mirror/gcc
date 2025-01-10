/* { dg-do run { target { s390_useable_hw } } } */
/* { dg-options "-O2 -march=z13 -mzarch -save-temps -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "\\.USUBC \\(" 4 "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump-times "\\.USUBC \\(" 2 "optimized" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\\tslbr\\t" 1 } } */
/* { dg-final { scan-assembler-times "\\tslb\\t" 1 } } */
/* { dg-final { scan-assembler-times "\\tslbgr\\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "\\tslbg\\t" 1 { target lp64 } } } */

#include <assert.h>

unsigned int __attribute__ ((noipa))
usubc (unsigned int x, unsigned int y, _Bool borrow_in, _Bool *borrow_out)
{
  unsigned int r;
  _Bool b1 = __builtin_sub_overflow (x, y, &r);
  _Bool b2 = __builtin_sub_overflow (r, borrow_in, &r);
  *borrow_out = b1 | b2;
  return r;
}

void
test_int (void)
{
  _Bool b;
  unsigned int r;

  r = usubc (0xfu, 0xfu, 0, &b);
  assert (r == 0 && !b);

  r = usubc (0xfu, 0xfu, 1, &b);
  assert (r == 0xffffffffu && b);

  r = usubc (0xfu, 0xffu, 0, &b);
  assert (r == 0xffffff10u && b);

  r = usubc (0xfu, 0xffu, 1, &b);
  assert (r == 0xffffff0fu && b);
}

unsigned int __attribute__ ((noipa))
usubc_mem (unsigned int *x, unsigned int *y, _Bool borrow_in, _Bool *borrow_out)
{
  unsigned int r;
  _Bool b1 = __builtin_sub_overflow (*x, *y, &r);
  _Bool b2 = __builtin_sub_overflow (r, borrow_in, &r);
  *borrow_out = b1 | b2;
  return r;
}

void
test_int_mem (void)
{
  _Bool b;
  unsigned int r, x, y;

  x = 0xfu;
  y = 0xfu;
  r = usubc_mem (&x, &y, 0, &b);
  assert (r == 0 && !b);

  x = 0xfu;
  y = 0xfu;
  r = usubc_mem (&x, &y, 1, &b);
  assert (r == 0xffffffffu && b);

  x = 0xfu;
  y = 0xffu;
  r = usubc_mem (&x, &y, 0, &b);
  assert (r == 0xffffff10u && b);

  x = 0xfu;
  y = 0xffu;
  r = usubc_mem (&x, &y, 1, &b);
  assert (r == 0xffffff0fu && b);
}

#ifdef __s390x__
unsigned long __attribute__ ((noipa))
usubcl (unsigned long x, unsigned long y, _Bool borrow_in, _Bool *borrow_out)
{
  unsigned long r;
  _Bool b1 = __builtin_sub_overflow (x, y, &r);
  _Bool b2 = __builtin_sub_overflow (r, borrow_in, &r);
  *borrow_out = b1 | b2;
  return r;
}

void
test_long (void)
{
  _Bool b;
  unsigned long r;

  r = usubcl (0xfu, 0xfu, 0, &b);
  assert (r == 0 && !b);

  r = usubcl (0xfu, 0xfu, 1, &b);
  assert (r == 0xffffffffffffffffu && b);

  r = usubcl (0xfu, 0xffu, 0, &b);
  assert (r == 0xffffffffffffff10u && b);

  r = usubcl (0xfu, 0xffu, 1, &b);
  assert (r == 0xffffffffffffff0fu && b);
}

unsigned long __attribute__ ((noipa))
usubcl_mem (unsigned long *x, unsigned long *y, _Bool borrow_in, _Bool *borrow_out)
{
  unsigned long r;
  _Bool b1 = __builtin_sub_overflow (*x, *y, &r);
  _Bool b2 = __builtin_sub_overflow (r, borrow_in, &r);
  *borrow_out = b1 | b2;
  return r;
}

void
test_long_mem (void)
{
  _Bool b;
  unsigned long r, x, y;

  x = 0xfu;
  y = 0xfu;
  r = usubcl_mem (&x, &y, 0, &b);
  assert (r == 0 && !b);

  x = 0xfu;
  y = 0xfu;
  r = usubcl_mem (&x, &y, 1, &b);
  assert (r == 0xffffffffffffffffu && b);

  x = 0xfu;
  y = 0xffu;
  r = usubcl_mem (&x, &y, 0, &b);
  assert (r == 0xffffffffffffff10u && b);

  x = 0xfu;
  y = 0xffu;
  r = usubcl_mem (&x, &y, 1, &b);
  assert (r == 0xffffffffffffff0fu && b);
}
#endif

int
main (void)
{
  test_int ();
  test_int_mem ();
#ifdef __s390x__
  test_long ();
  test_long_mem ();
#endif
  return 0;
}
