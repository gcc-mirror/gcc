/* Test v2sf calculations.  The nmadd and nmsub patterns need
   -ffinite-math-only.  */
/* { dg-do compile } */
/* { dg-options "(HAS_MADDPS) -mmadd4 -mgp32 -mpaired-single -ffinite-math-only forbid_cpu=octeon.*" } */
/* { dg-skip-if "nmadd and nmsub need combine" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tcvt.ps.s\t" } } */
/* { dg-final { scan-assembler "\tmov.ps\t" } } */
/* { dg-final { scan-assembler "\tldc1\t" } } */
/* { dg-final { scan-assembler "\tsdc1\t" } } */
/* { dg-final { scan-assembler "\tadd.ps\t" } } */
/* { dg-final { scan-assembler "\tsub.ps\t" } } */
/* { dg-final { scan-assembler "\tneg.ps\t" } } */
/* { dg-final { scan-assembler "\tmul.ps\t" } } */
/* { dg-final { scan-assembler "\tmadd.ps\t" } } */
/* { dg-final { scan-assembler "\tmsub.ps\t" } } */
/* { dg-final { scan-assembler "\tnmadd.ps\t" } } */
/* { dg-final { scan-assembler "\tnmsub.ps\t" } } */
/* { dg-final { scan-assembler "\tmovn.ps\t" } } */
/* { dg-final { scan-assembler "\tmovz.ps\t" } } */

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
  register v2sf b __asm__("$f0") = a;
  register v2sf c __asm__("$f2");
  __asm__ __volatile__ ("" : "+f" (b));
  c = b;
  __asm__ __volatile__ ("" : : "f" (c));
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
