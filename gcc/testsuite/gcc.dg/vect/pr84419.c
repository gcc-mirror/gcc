#include <string.h>

#define SIZE 400

int  foo[SIZE];
char bar[SIZE];

void __attribute__ ((noinline)) foo_func(void)
{
  int i;
  for (i = 1; i < SIZE; i++)
    if (bar[i])
      foo[i] = 1;
}

int main()
{
  memset(bar, 1, sizeof(bar));
  foo_func();
  return 0;
}
