/* PR c/52577 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

typedef int V __attribute__((vector_size (sizeof (int) * 4)));

void
f1 (V *p)
{
  V mask = { 1, 2, 3, 0 };
  *p = __builtin_shuffle (*p, mask);
}

void
f2 (V *p, V *q)
{
  V mask = { 1, 2, 3, 0 };
  *p = __builtin_shuffle (*p, *q, mask);
}

void
f3 (V *p, V *mask)
{
  V a = { 1, 2, 3, 0 };
  *p = __builtin_shuffle (a, *mask);
}

void
f4 (V *p, V *mask)
{
  V a = { 1, 2, 3, 0 };
  V b = { 2, 3, 4, 1 };
  *p = __builtin_shuffle (a, b, *mask);
}
