/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int addu (unsigned int a, unsigned int b)
{
  unsigned int out;
  unsigned int overflow = __builtin_add_overflow (a, b, &out);
  return overflow & out;
}

int addi (int a, int b)
{
  int out;
  int overflow = __builtin_add_overflow (a, b, &out);
  return overflow & out;
}

unsigned int subu (unsigned int a, unsigned int b)
{
  unsigned int out;
  unsigned int overflow = __builtin_sub_overflow (a, b, &out);
  return overflow & out;
}

int subi (int a, int b)
{
  int out;
  int overflow = __builtin_sub_overflow (a, b, &out);
  return overflow & out;
}

/* { dg-final { scan-assembler-not "sext\.w\t" } } */
