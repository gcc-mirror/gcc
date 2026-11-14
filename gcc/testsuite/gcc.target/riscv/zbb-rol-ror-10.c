/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

unsigned long
rol_neg (unsigned long x, unsigned long n)
{
  return __builtin_stdc_rotate_left (x, 0UL - (n & 255UL));
}

unsigned long
ror_neg (unsigned long x, unsigned long n)
{
  return __builtin_stdc_rotate_right (x, 0UL - (n & 255UL));
}

/* This mask changes the effective count and must be retained.  */
unsigned long
ror_neg_mask7 (unsigned long x, unsigned long n)
{
  return __builtin_stdc_rotate_right (x, 0UL - (n & 7UL));
}

#if __riscv_xlen == 64
unsigned int
rolw_neg (unsigned int x, unsigned int n)
{
  return __builtin_stdc_rotate_left (x, 0U - (n & 31U));
}

unsigned int
rorw_neg (unsigned int x, unsigned int n)
{
  return __builtin_stdc_rotate_right (x, 0U - (n & 31U));
}
#endif

/* { dg-final { scan-assembler-times "rol\t" 2 } } */
/* { dg-final { scan-assembler-times "ror\t" 1 } } */
/* { dg-final { scan-assembler-times "rolw\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "rorw\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-not "negw?\t" } } */
