/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-minline-strcmp -minline-strncmp -march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-options "-minline-strcmp -minline-strncmp -march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

typedef long unsigned int size_t;

int
my_str_cmp (const char *s1, const char *s2)
{
  return __builtin_strcmp (s1, s2);
}

int
my_str_cmp_const (const char *s1)
{
  return __builtin_strcmp (s1, "foo");
}

int
my_strn_cmp (const char *s1, const char *s2, size_t n)
{
  return __builtin_strncmp (s1, s2, n);
}

int
my_strn_cmp_const (const char *s1, size_t n)
{
  return __builtin_strncmp (s1, "foo", n);
}

int
my_strn_cmp_bounded (const char *s1, const char *s2)
{
  return __builtin_strncmp (s1, s2, 42);
}

/* { dg-final { scan-assembler-not "orc.b\t" } } */
