/* PR target/109140 */
/* { dg-do compile } */
/* { dg-options "-O3 -mvis4 -std=c99" } */

#define TYPE char

struct S { TYPE ub[8]; };

struct S s;

TYPE v;

void foo (void)
{
  for (int i = 0; i < 8; i++)
    s.ub[i] = s.ub[i] > v;
}

/* { dg-final { scan-assembler "fpcmpgt8\t%" } } */
