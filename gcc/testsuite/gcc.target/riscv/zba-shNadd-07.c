/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64 -O2" } */

unsigned long
f1 (unsigned long i)
{
  return i * 200;
}

unsigned long
f2 (unsigned long i)
{
  return i * 783;
}

unsigned long
f3 (unsigned long i)
{
  return i * 784;
}

unsigned long
f4 (unsigned long i)
{
  return i * 1574;
}

/* { dg-final { scan-assembler-times {\msh2add} 2 } } */
/* { dg-final { scan-assembler-times {\msh1add} 1 } } */
/* { dg-final { scan-assembler-times {\mslli} 3 } } */
/* { dg-final { scan-assembler-times {\mmul} 2 } } */
