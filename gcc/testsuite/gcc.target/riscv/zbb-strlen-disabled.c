/* { dg-do compile } */
/* { dg-options "-mno-inline-strlen -march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-options "-mno-inline-strlen -march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

typedef long unsigned int size_t;

size_t
my_str_len (const char *s)
{
  s = __builtin_assume_aligned (s, 4096);
  return __builtin_strlen (s);
}

/* { dg-final { scan-assembler-not "orc.b\t" } } */
