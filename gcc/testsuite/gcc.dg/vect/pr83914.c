/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

struct s { struct s *ptrs[16]; } *a, *b;
int c;
void
foo (int n)
{
  for (; n; a = b, n--)
    {
      b = a + 1;
      for (c = 8; c; c--)
	a->ptrs[c] = b;
    }
}
