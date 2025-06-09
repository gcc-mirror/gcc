/* { dg-do run } */
/* PR tree-optimization/121422 */

struct s1
{
  char a[4];
};
struct s1 b;
char t[4];

/* if both t and b startout zero initialized before this function,
   t should end up being:
   {0, 0, 1, 0}
   while b.a should end up being:
   {0, 0, 0, 1}
*/
__attribute__((noipa,noinline))
void f(void)
{
  __builtin_memset(&b.a[1], 0, 2);
  b.a[3] = 1;
  /* This memcpy should stay a memcpy and not become memset. */
  __builtin_memcpy(&t[0], &b.a[1], 3);
}


int main()
{
  f();
  for(int i = 0; i < 4; i++)
  {
        if (t[i] != (i == 2 ? 1 : 0))
          __builtin_abort();
  }
}

