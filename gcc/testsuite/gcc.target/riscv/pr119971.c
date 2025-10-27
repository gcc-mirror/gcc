/* { dg-do compile { target rv64 } } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" "-Oz" "-Os" } } */

__attribute__ ((noipa)) unsigned
foo (unsigned b, unsigned e, unsigned i)
{
  e >>= b;
  i >>= e & 31;
  return i & 1;
}

int main()
{
  if (foo (0x18, 0xfe000000, 0x40000000) != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-times "srlw\t" 2 } } */
/* { dg-final { scan-assembler-not "bext\t" } } */


