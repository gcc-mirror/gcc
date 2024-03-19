/* { dg-do compile } */
/* { dg-skip-if "jli only available for ARCv2" { arc700 || arc6xx } } */
/* { dg-options "-O0 -mcode-density" } */

int func(int i) __attribute__((jli_fixed(2)));

int func(int i)
{
  return i*i;
}

int main()
{
  return func(100);
}

/* { dg-final { scan-assembler "jli_s\\\s+2" } } */
/* { dg-final { scan-assembler-not ".weak __jli.func" } } */
/* { dg-final { scan-assembler-not "b\\\s+@func" } } */
