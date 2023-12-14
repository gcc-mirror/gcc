/* { dg-options "-O2 -fno-rename-registers" } */

#include <arm_sve.h>

#define ACCUMULATE(VAR, OP)					\
  do								\
    {								\
      VAR = OP (pg, VAR, svld1 (pg, ptr1), svld1 (pg, ptr2));	\
      ptr1 += svcntw ();					\
      ptr2 += svcntw ();					\
    }								\
  while (0)

svint32_t
f1 (svint32_t x, int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    ACCUMULATE (x, svmla_x);
  return x;
}

svint32_t
f2 (svint32_t x, int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x, svmla_x);
      ACCUMULATE (x, svmls_x);
    }
  return x;
}

svint32_t
f3 (svint32_t x, int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x, svmla_x);
      ACCUMULATE (x, svmls_x);
      ACCUMULATE (x, svmad_x);
      ACCUMULATE (x, svmsb_x);
    }
  return x;
}

void
f4 (svint32_t x0, svint32_t x1, svint32_t x2, svint32_t x3,
    int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmla_x);
      ACCUMULATE (x1, svmla_x);
      ACCUMULATE (x2, svmla_x);
      ACCUMULATE (x3, svmla_x);
    }
  svst1_vnum (pg, ptr1, 0, x0);
  svst1_vnum (pg, ptr1, 1, x1);
  svst1_vnum (pg, ptr1, 2, x2);
  svst1_vnum (pg, ptr1, 3, x3);
}

void
f5 (svint32_t x0, svint32_t x1, svint32_t x2, svint32_t x3,
    int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmla_x);
      ACCUMULATE (x1, svmla_x);
      ACCUMULATE (x2, svmla_x);
      ACCUMULATE (x3, svmla_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmls_x);
      ACCUMULATE (x1, svmls_x);
      ACCUMULATE (x2, svmls_x);
      ACCUMULATE (x3, svmls_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmad_x);
      ACCUMULATE (x1, svmad_x);
      ACCUMULATE (x2, svmad_x);
      ACCUMULATE (x3, svmad_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmsb_x);
      ACCUMULATE (x1, svmsb_x);
      ACCUMULATE (x2, svmsb_x);
      ACCUMULATE (x3, svmsb_x);
    }
  svst1_vnum (pg, ptr1, 0, x0);
  svst1_vnum (pg, ptr1, 1, x1);
  svst1_vnum (pg, ptr1, 2, x2);
  svst1_vnum (pg, ptr1, 3, x3);
}

void
f6 (svint32_t x0, svint32_t x1, svint32_t x2, svint32_t x3,
    int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmla_x);
      ACCUMULATE (x1, svmla_x);
      ACCUMULATE (x2, svmla_x);
      ACCUMULATE (x3, svmla_x);
      ACCUMULATE (x0, svmls_x);
      ACCUMULATE (x1, svmls_x);
      ACCUMULATE (x2, svmls_x);
      ACCUMULATE (x3, svmls_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmad_x);
      ACCUMULATE (x1, svmad_x);
      ACCUMULATE (x2, svmad_x);
      ACCUMULATE (x3, svmad_x);
      ACCUMULATE (x0, svmsb_x);
      ACCUMULATE (x1, svmsb_x);
      ACCUMULATE (x2, svmsb_x);
      ACCUMULATE (x3, svmsb_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmad_x);
      ACCUMULATE (x1, svmad_x);
      ACCUMULATE (x2, svmad_x);
      ACCUMULATE (x3, svmad_x);
      ACCUMULATE (x0, svmsb_x);
      ACCUMULATE (x1, svmsb_x);
      ACCUMULATE (x2, svmsb_x);
      ACCUMULATE (x3, svmsb_x);
    }
  svst1_vnum (pg, ptr1, 0, x0);
  svst1_vnum (pg, ptr1, 1, x1);
  svst1_vnum (pg, ptr1, 2, x2);
  svst1_vnum (pg, ptr1, 3, x3);
}

void
f7 (svint32_t x0, svint32_t x1, svint32_t x2, svint32_t x3,
    int32_t *ptr1, int32_t *ptr2)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmla_x);
      ACCUMULATE (x1, svmla_x);
      ACCUMULATE (x2, svmla_x);
      ACCUMULATE (x3, svmla_x);
      ACCUMULATE (x0, svmls_x);
      ACCUMULATE (x1, svmls_x);
      ACCUMULATE (x2, svmls_x);
      ACCUMULATE (x3, svmls_x);
      ACCUMULATE (x0, svmad_x);
      ACCUMULATE (x1, svmad_x);
      ACCUMULATE (x2, svmad_x);
      ACCUMULATE (x3, svmad_x);
      ACCUMULATE (x0, svmsb_x);
      ACCUMULATE (x1, svmsb_x);
      ACCUMULATE (x2, svmsb_x);
      ACCUMULATE (x3, svmsb_x);
    }
  for (int i = 0; i < 100; ++i)
    {
      ACCUMULATE (x0, svmla_x);
      ACCUMULATE (x1, svmla_x);
      ACCUMULATE (x2, svmla_x);
      ACCUMULATE (x3, svmla_x);
      ACCUMULATE (x0, svmls_x);
      ACCUMULATE (x1, svmls_x);
      ACCUMULATE (x2, svmls_x);
      ACCUMULATE (x3, svmls_x);
      ACCUMULATE (x0, svmad_x);
      ACCUMULATE (x1, svmad_x);
      ACCUMULATE (x2, svmad_x);
      ACCUMULATE (x3, svmad_x);
      ACCUMULATE (x0, svmsb_x);
      ACCUMULATE (x1, svmsb_x);
      ACCUMULATE (x2, svmsb_x);
      ACCUMULATE (x3, svmsb_x);
    }
  svst1_vnum (pg, ptr1, 0, x0);
  svst1_vnum (pg, ptr1, 1, x1);
  svst1_vnum (pg, ptr1, 2, x2);
  svst1_vnum (pg, ptr1, 3, x3);
}

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
