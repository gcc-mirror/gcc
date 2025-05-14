// PR c++/44613
// { dg-do run }
// { dg-options "" }

typedef int int32_t __attribute__((mode (__SI__)));

void *volatile p;

int
main (void)
{
  int32_t n = 0;
 lab:;
  int x[n % 1000 + 1];
  x[0] = 1;
  x[n % 1000] = 2;
  p = x;
  n++;
  if (n < 1000000)
    goto lab;
  return 0;
}
