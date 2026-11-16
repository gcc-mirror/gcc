/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

unsigned int
rol (unsigned int x, unsigned short n)
{
  return __builtin_stdc_rotate_left (x, n & 31);
}

unsigned int
ror (unsigned int x, unsigned short n)
{
  return __builtin_stdc_rotate_right (x, n & 31);
}

long
rol_sext (unsigned int x, unsigned short n)
{
  return (int) __builtin_stdc_rotate_left (x, n & 31);
}

long
ror_sext (unsigned int x, unsigned short n)
{
  return (int) __builtin_stdc_rotate_right (x, n & 31);
}

unsigned long
rol_native (unsigned long x, unsigned short n)
{
  return __builtin_stdc_rotate_left (x, n & (__riscv_xlen - 1));
}

unsigned long
ror_native (unsigned long x, unsigned short n)
{
  return __builtin_stdc_rotate_right (x, n);
}

/* A narrower mask must be retained.  */
unsigned int
ror_mask7 (unsigned int x, unsigned short n)
{
  return __builtin_stdc_rotate_right (x, n & 7);
}

/* { dg-final { scan-assembler-times "rolw?\t" 3 } } */
/* { dg-final { scan-assembler-times "rorw?\t" 4 } } */
/* { dg-final { scan-assembler-times "andi\t" 1 } } */
