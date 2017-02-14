/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

#include <stdio.h>

struct container
{
  int x;
  int y;
};

static struct container max;
static int array[3][3];
static int array2[123];

__attribute__ ((noinline))
void foo(void)
{
  printf("Foo()");
}

__attribute__ ((noinline))
int order(int x, int y)
{
  return x < y ? 2 : 4;
}

__attribute__ ((noinline))
int order2(int y, int x)
{
  return x < y ? 2 : 4;
}

__attribute__ ((noinline))
void x1(int x)
{
  int i;
  for(i = 0; i < 20; ++i)
    array2[i] = i;

  array2[2] = 13;
}

__attribute__ ((noinline))
void x2(int a)
{
  int i;
  for(i = 0; i < 20; ++i)
    array2[i] = i;

  array2[2] = 13;
}

int main(int argc, char **argv)
{
  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:x1->x2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
