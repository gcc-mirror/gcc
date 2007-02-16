/* PR rtl-optimization/30787 */
/* Testcase by Jakub Jelinek <jakub@gcc.gnu.org> */

struct S
{
  int *s;
};

void test (int x, struct S *y)
{
  int i;
  for (i = 0; i < x; i++)
    {
      if (y)
        y->s[i] += 1;
    }
}

int main (void)
{
  test (1, (void *) 0);
  return 0;
}
