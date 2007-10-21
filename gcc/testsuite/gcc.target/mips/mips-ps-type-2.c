/* Test v2sf calculations.  The nmadd and nmsub patterns need
   -ffinite-math-only.  */
/* { dg-do compile } */
/* { dg-mips-options "-mips32r2 -O2 -mpaired-single -ffinite-math-only" } */
/* { dg-final { scan-assembler "cvt.ps.s" } } */
/* { dg-final { scan-assembler "mov.ps" } } */
/* { dg-final { scan-assembler "ldc1" } } */
/* { dg-final { scan-assembler "sdc1" } } */
/* { dg-final { scan-assembler "add.ps" } } */
/* { dg-final { scan-assembler "sub.ps" } } */
/* { dg-final { scan-assembler "neg.ps" } } */
/* { dg-final { scan-assembler "mul.ps" } } */
/* { dg-final { scan-assembler "madd.ps" } } */
/* { dg-final { scan-assembler "msub.ps" } } */
/* { dg-final { scan-assembler "nmadd.ps" } } */
/* { dg-final { scan-assembler "nmsub.ps" } } */
/* { dg-final { scan-assembler "movn.ps" } } */
/* { dg-final { scan-assembler "movz.ps" } } */

typedef float v2sf __attribute__ ((vector_size(8)));
void gobble (v2sf);

v2sf A = {100, 200};

/* Init from  floats */
NOMIPS16 v2sf init (float a, float b)
{
  return (v2sf) {a, b};
}

/* Move between registers */
NOMIPS16 v2sf move (v2sf a)
{
  return a;
}

/* Load from memory */
NOMIPS16 v2sf load ()
{
  return A;
}

/* Store to memory */
NOMIPS16 void store (v2sf a)
{
  A = a;
}

/* Add */
NOMIPS16 v2sf add (v2sf a, v2sf b)
{
  return a + b;
}

/* Subtract */
NOMIPS16 v2sf sub (v2sf a, v2sf b)
{
  return a - b;
}

/* Negate */
NOMIPS16 v2sf neg (v2sf a)
{
  return - a;
}

/* Multiply */
NOMIPS16 v2sf mul (v2sf a, v2sf b)
{
  return a * b;
}

/* Multiply and add */
NOMIPS16 v2sf madd (v2sf a, v2sf b, v2sf c)
{
  return a * b + c;
}

/* Multiply and subtract */
NOMIPS16 v2sf msub (v2sf a, v2sf b, v2sf c)
{
  return a * b - c;
}

/* Negate Multiply and add */
NOMIPS16 v2sf nmadd (v2sf a, v2sf b, v2sf c)
{
  return - (a * b + c);
}

/* Negate Multiply and subtract */
NOMIPS16 v2sf nmsub (v2sf a, v2sf b, v2sf c)
{
  return - (a * b - c);
}

/* Conditional Move */
NOMIPS16 v2sf cond_move1 (v2sf a, v2sf b, int i)
{
  if (i == 0)
    a = b;
  gobble (a);
}

/* Conditional Move */
NOMIPS16 v2sf cond_move2 (v2sf a, v2sf b, int i)
{
  if (i != 0)
    a = b;
  gobble (a);
}
