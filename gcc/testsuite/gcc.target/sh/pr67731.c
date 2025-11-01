/* { dg-do compile } */
/* { dg-options "-O2 -m4 -ml" } */

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

/* { dg-final { scan-assembler-times {\ttst} 2 } } */
/* { dg-final { scan-assembler-times {\tnegc} 2 } } */
/* { dg-final { scan-assembler-not {\tor} } } */
