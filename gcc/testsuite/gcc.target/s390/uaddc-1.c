/* { dg-do run { target { s390_useable_hw } } } */
/* { dg-options "-O2 -march=z13 -mzarch -save-temps -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "\\.UADDC \\(" 4 "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump-times "\\.UADDC \\(" 2 "optimized" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\\talcr\\t" 1 } } */
/* { dg-final { scan-assembler-times "\\talc\\t" 1 } } */
/* { dg-final { scan-assembler-times "\\talcgr\\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "\\talcg\\t" 1 { target lp64 } } } */

#include <assert.h>

unsigned int __attribute__ ((noipa))
uaddc (unsigned int x, unsigned int y, _Bool carry_in, _Bool *carry_out)
{
  unsigned int r;
  _Bool c1 = __builtin_add_overflow (x, y, &r);
  _Bool c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 | c2;
  return r;
}

void
test_int (void)
{
  _Bool c;
  unsigned int r;

  r = uaddc (0xf0000000u, 0x0fffffffu, 0, &c);
  assert (r == 0xffffffffu && !c);

  r = uaddc (0xf0000000u, 0x0fffffffu, 1, &c);
  assert (r == 0 && c);

  r = uaddc (0xf0000001u, 0x0fffffffu, 0, &c);
  assert (r == 0 && c);

  r = uaddc (0xf0000001u, 0x0fffffffu, 1, &c);
  assert (r == 1 && c);
}

unsigned int __attribute__ ((noipa))
uaddc_mem (unsigned int *x, unsigned int *y, _Bool carry_in, _Bool *carry_out)
{
  unsigned int r;
  _Bool c1 = __builtin_add_overflow (*x, *y, &r);
  _Bool c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 | c2;
  return r;
}

void
test_int_mem (void)
{
  _Bool c;
  unsigned int r, x, y;

  x = 0xf0000000u;
  y = 0x0fffffffu;
  r = uaddc_mem (&x, &y, 0, &c);
  assert (r == 0xffffffffu && !c);

  x = 0xf0000000u;
  y = 0x0fffffffu;
  r = uaddc_mem (&x, &y, 1, &c);
  assert (r == 0 && c);

  x = 0xf0000001u;
  y = 0x0fffffffu;
  r = uaddc_mem (&x, &y, 0, &c);
  assert (r == 0 && c);

  x = 0xf0000001u;
  y = 0x0fffffffu;
  r = uaddc_mem (&x, &y, 1, &c);
  assert (r == 1 && c);
}

#ifdef __s390x__
unsigned long __attribute__ ((noipa))
uaddcl (unsigned long x, unsigned long y, _Bool carry_in, _Bool *carry_out)
{
  unsigned long r;
  _Bool c1 = __builtin_add_overflow (x, y, &r);
  _Bool c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 | c2;
  return r;
}

void
test_long (void)
{
  _Bool c;
  unsigned long r;

  r = uaddcl (0xf000000000000000u, 0x0fffffffffffffffu, 0, &c);
  assert (r == 0xffffffffffffffffu && !c);

  r = uaddcl (0xf000000000000000u, 0x0fffffffffffffffu, 1, &c);
  assert (r == 0 && c);

  r = uaddcl (0xf000000000000001u, 0x0fffffffffffffffu, 0, &c);
  assert (r == 0 && c);

  r = uaddcl (0xf000000000000001u, 0x0fffffffffffffffu, 1, &c);
  assert (r == 1 && c);
}

unsigned long __attribute__ ((noipa))
uaddcl_mem (unsigned long *x, unsigned long *y, _Bool carry_in, _Bool *carry_out)
{
  unsigned long r;
  _Bool c1 = __builtin_add_overflow (*x, *y, &r);
  _Bool c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 | c2;
  return r;
}

void
test_long_mem (void)
{
  _Bool c;
  unsigned long r, x, y;

  x = 0xf000000000000000u;
  y = 0x0fffffffffffffffu;
  r = uaddcl_mem (&x, &y, 0, &c);
  assert (r == 0xffffffffffffffffu && !c);

  x = 0xf000000000000000u;
  y = 0x0fffffffffffffffu;
  r = uaddcl_mem (&x, &y, 1, &c);
  assert (r == 0 && c);

  x = 0xf000000000000001u;
  y = 0x0fffffffffffffffu;
  r = uaddcl_mem (&x, &y, 0, &c);
  assert (r == 0 && c);

  x = 0xf000000000000001u;
  y = 0x0fffffffffffffffu;
  r = uaddcl_mem (&x, &y, 1, &c);
  assert (r == 1 && c);
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
