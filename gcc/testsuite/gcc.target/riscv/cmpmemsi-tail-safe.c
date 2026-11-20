/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */
/* { dg-additional-options "-ffat-lto-objects" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** cmp3:
**  ...
**  lhu?\s+[^\n]+,0\([^)]+\)
**  ...
**  lhu?\s+[^\n]+,0\([^)]+\)
**  ...
**  lbu\s+[^\n]+,2\([^)]+\)
**  ...
**  lbu\s+[^\n]+,2\([^)]+\)
**  ...
*/
int
cmp3 (const void *a, const void *b)
{
  a = __builtin_assume_aligned (a, 8);
  b = __builtin_assume_aligned (b, 8);
  return __builtin_memcmp (a, b, 3);
}

int
cmp5 (const void *a, const void *b)
{
  a = __builtin_assume_aligned (a, 8);
  b = __builtin_assume_aligned (b, 8);
  return __builtin_memcmp (a, b, 5);
}

int
cmp6 (const void *a, const void *b)
{
  a = __builtin_assume_aligned (a, 8);
  b = __builtin_assume_aligned (b, 8);
  return __builtin_memcmp (a, b, 6);
}

int
cmp7 (const void *a, const void *b)
{
  a = __builtin_assume_aligned (a, 8);
  b = __builtin_assume_aligned (b, 8);
  return __builtin_memcmp (a, b, 7);
}

/* None of these comparisons may use an eight-byte load.  */
/* { dg-final { scan-assembler-not "\\tld\\t" } } */
/* { dg-final { scan-assembler-not "\\t(call|tail)\\tmemcmp" } } */
