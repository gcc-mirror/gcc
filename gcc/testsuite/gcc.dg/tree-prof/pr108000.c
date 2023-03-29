/* { dg-options "-O2" } */

#include <stdlib.h>

volatile int flag;
const int array_size = 10;
int* array;
int iterations = 10000000;

#define BAR(num) \
int __attribute__((noinline)) \
bar##num (int i, int j) \
{ \
  if (i == 0) \
    return 2*num - 1; \
  else \
    return 2*num; \
}

BAR(1)
BAR(2)
BAR(3)
BAR(4)
BAR(5)
BAR(6)
BAR(7)
BAR(8)
BAR(9)
BAR(10)
BAR(11)
BAR(12)
BAR(13)
BAR(14)
BAR(15)
BAR(16)
BAR(17)
BAR(18)
BAR(19)

int __attribute__((noinline))
foo ()
{
  switch (flag)
  {
	case 1:
		return bar1 (0, 0);
	case 2:
		return bar2 (0, 0);
	case 3:
		return bar3 (0, 0);
	case 4:
		return bar4 (0, 0);
	case 5:
		return bar5 (0, 0);
	case 6:
		return bar6 (0, 0);
	case 7:
		return bar7 (0, 0);
	case 8:
		return bar8 (0, 0);
	case 9:
		return bar9 (0, 0);
	case 10:
		return bar10 (0, 0);
	case 11:
		return bar11 (0, 0);
	case 12:
		return bar12 (0, 0);
	case 13:
		return bar13 (0, 0);
	case 14:
		return bar14 (0, 0);
	case 15:
		return bar15 (0, 0);
	case 16:
		return bar16 (0, 0);
	case 17:
		return bar17 (0, 0);
	case 18:
		return bar18 (0, 0);
	default:
		return bar19(0, 0);
  }
}

int
main ()
{
  flag = 0;
  array = calloc(array_size, sizeof(int));
  for (int i = 0, j = 0; i < iterations; ++i, j = (j + 1) % 10)
    array[j] = foo ();
}
