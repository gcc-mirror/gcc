/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-minline-strlen -march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-options "-minline-strlen -march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

typedef long unsigned int size_t;

size_t
my_str_len (const char *s)
{
  return __builtin_strlen (s);
}

/* { dg-final { scan-assembler-not "orc.b\t" } } */
