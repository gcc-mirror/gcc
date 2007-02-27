/* PR rtl-optimization/30931 */
/* Testcase by Peter Bergner <bergner@gcc.gnu.org> */

struct s
{
  int first;
  int done;
};

void bug (struct s *p)
{
  int i;
  for (i=0; i < 2; i++)
    {
      while (p[i].first && p[i].done)
        p[i].first = 0;
    }
}

int main (void)
{
  struct s array[2];
  array[0].first = 1;
  array[0].done = 1;
  array[1].first = 0;
  array[1].done = 0;

  bug (array);

  return 0;
}
