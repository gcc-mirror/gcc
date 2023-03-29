/* PR target/109140 */
/* { dg-do compile } */
/* { dg-options "-O3 -mvis3 -std=c99" } */

#define TYPE unsigned char

struct S { TYPE ub[8]; };

struct S s;

TYPE v;

void foo (void)
{
  for (int i = 0; i < 8; i++)
    s.ub[i] = s.ub[i] > v;
}

/* { dg-final { scan-assembler "fucmpgt8\t%" } } */
