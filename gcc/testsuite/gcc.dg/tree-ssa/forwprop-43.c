/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-additional-options "-fgimple" } */

#include <stdint.h>

typedef int32_t int32x4_t __attribute__((vector_size(16)));
typedef int32_t int32x2_t __attribute__((vector_size(8)));
typedef int32_t int32x1_t __attribute__((vector_size(4)));

int32x4_t __GIMPLE (ssa)
foo (int32x4_t x)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = __BIT_FIELD_REF <int32x2_t> (x, 64, 0);
  _6 = _Literal (int32x4_t) { _1, _2 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo2 (int32x4_t x)
{
  int32x1_t _1;
  int32x1_t _2;
  int32x1_t _3;
  int32x1_t _4;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x1_t> (x, 32, 64);
  _2 = __BIT_FIELD_REF <int32x1_t> (x, 32, 96);
  _3 = __BIT_FIELD_REF <int32x1_t> (x, 32, 0);
  _4 = __BIT_FIELD_REF <int32x1_t> (x, 32, 32);
  _6 = _Literal (int32x4_t) { _1, _2, _3, _4 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo3 (int32x4_t x, int32x4_t y)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = __BIT_FIELD_REF <int32x2_t> (y, 64, 0);
  _6 = _Literal (int32x4_t) { _1, _2 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo4 (int32x4_t x, int32x4_t y)
{
  int32x1_t _1;
  int32x1_t _2;
  int32x1_t _3;
  int32x1_t _4;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x1_t> (x, 32, 64);
  _2 = __BIT_FIELD_REF <int32x1_t> (y, 32, 96);
  _3 = __BIT_FIELD_REF <int32x1_t> (x, 32, 0);
  _4 = __BIT_FIELD_REF <int32x1_t> (y, 32, 32);
  _6 = _Literal (int32x4_t) { _1, _2, _3, _4 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo5 (int32x4_t x)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = _Literal (int32x2_t) { 1, 2 };
  _6 = _Literal (int32x4_t) { _1, _2 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo6 (int32x4_t x, int32_t y)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = _Literal (int32x2_t) { y, y };
  _6 = _Literal (int32x4_t) { _1, _2 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo7 (int32x4_t x)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = _Literal (int32x2_t) { 1, 2 };
  _6 = _Literal (int32x4_t) { _2, _1 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo8 (int32x4_t x, int32_t y)
{
  int32x2_t _1;
  int32x2_t _2;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x2_t> (x, 64, 64);
  _2 = _Literal (int32x2_t) { y, y };
  _6 = _Literal (int32x4_t) { _2, _1 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo9 (int32x4_t x)
{
  int32x1_t _1;
  int32x1_t _2;
  int32x1_t _3;
  int32x1_t _4;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x1_t> (x, 32, 96);
  _2 = __BIT_FIELD_REF <int32x1_t> (x, 32, 64);
  _3 = _Literal (int32x1_t) { 1 };
  _4 = _Literal (int32x1_t) { 1 };
  _6 = _Literal (int32x4_t) { _3, _4, _1, _2 };
  return _6;
}

int32x4_t __GIMPLE (ssa)
foo10 (int32x4_t x, int32_t y)
{
  int32x1_t _1;
  int32x1_t _2;
  int32x1_t _3;
  int32x1_t _4;
  int32x4_t _6;

__BB(2):
  _1 = __BIT_FIELD_REF <int32x1_t> (x, 32, 96);
  _2 = __BIT_FIELD_REF <int32x1_t> (x, 32, 64);
  _3 = _Literal (int32x1_t) { y };
  _4 = _Literal (int32x1_t) { y };
  _6 = _Literal (int32x4_t) { _3, _4, _1, _2 };

  return _6;
}


/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 10 "forwprop1" } } */
