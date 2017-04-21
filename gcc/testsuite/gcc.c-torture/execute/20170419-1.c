/* PR tree-optimization/80426 */
/* Testcase by <ishiura-compiler@ml.kwansei.ac.jp> */

#define INT_MAX 0x7fffffff
#define INT_MIN (-INT_MAX-1)

int x;

int main (void)
{
  volatile int a = 0;
  volatile int b = -INT_MAX;
  int j;

  for(j = 0; j < 18; j += 1) {
    x = ( (a == 0) != (b - (int)(INT_MIN) ) );
  }

  if (x != 0)
    __builtin_abort ();

  return 0;
}

