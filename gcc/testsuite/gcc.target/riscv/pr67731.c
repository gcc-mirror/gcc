/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcbv -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gcbv -mabi=ilp32" { target { rv32 } } } */

typedef struct
{
  _Bool a : 1;
  _Bool b : 1;
  _Bool c : 1;
  _Bool d : 1;
  unsigned int e : 4;
} S;

_Bool test_00 (S* s)
{
  return s->b | s->c;
}

_Bool test_01 (S* s)
{
  return s->b | s->c | s->d;
}
/* { dg-final { scan-assembler-times {\tlw\ta0,0\(a0\).*?\n\tandi\ta0,a0,\d+.*?\n\tsnez\ta0,a0.*?\n\tret} 2 } } */
/* { dg-final { scan-assembler-not {\tor} } } */
/* { dg-final { scan-assembler-not {\tbexti} } } */
