/* PR target/128035 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -w" } */
/* { dg-final { scan-assembler-times "call\[ \t\]+_?memcmp" 2 } } */

int
f (char *p1, char *p2)
{
  return __builtin_memcmp (p1, p2, -1) == 0;
}

int
g (char *p1, char *p2)
{
  return __builtin_bcmp (p1, p2, -1) == 0;
}
