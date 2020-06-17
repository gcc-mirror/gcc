/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized-all-all"  } */

#include <stdlib.h>
#include <stdio.h>

int gcd(int x, int y) __attribute__ ((pure));

__attribute__ ((noinline))
int gcd(int x, int y)
{
  int swap;

  if(x <= 0 || y <= 0)
    return 0;

  if(x < y)
    {
      swap = x;
      x = y;
      y = swap;
    }

  while(x != y)
    {
      x = x - y;

      if(y > x)
	{
	  swap = x;
	  x = y;
	  y = swap;
	}
    }

  return x;
}

int nsd(int x, int y) __attribute__ ((pure));

__attribute__ ((noinline))
int nsd(int x, int y)
{
  int swap;

  if(x <= 0 || y <= 0)
    return 0;

  if(x < y)
    {
      swap = x;
      x = y;
      y = swap;
    }

  while(x != y)
    {
      x = x - y;

      if(y > x)
	{
	  swap = x;
	  x = y;
	  y = swap;
	}
    }

  return x;
}

int nsd_different_result(int x, int y) __attribute__ ((pure));

__attribute__ ((noinline))
int nsd_different_result(int x, int y)
{
  int pes;

  if(x <= 0 || y <= 0)
    return 1;

  if(x < 10)
    y = 12;
  else if(x == 44)
    y = 124;
  else
    y = 1111;

  if(x < y)
    {
      pes = x;
      x = y;
      y = pes;
    }

  while(x != y)
    {
      x = x - y;

      if(y > x)
	{
	  pes = x;
	  x = y;
	  y = pes;
	}
    }

  return x;
}

int nsd_different_result2(int x, int y) __attribute__ ((pure));

__attribute__ ((noinline))
int nsd_different_result2(int x, int y)
{
  int pes;

  if(x <= 0 || y <= 0)
    return 1;

  if(x < 10)
    y = 12;
  else if(x == 44)
    y = 124;
  else
    y = 1111;

  if(x < y)
    {
      pes = x;
      x = y;
      y = pes;
    }

  while(x != y)
    {
      x = x - y;

      if(y > x)
	{
	  pes = x;
	  x = y;
	  y = pes;
	}
    }

  return x;
}

__attribute__ ((noinline))
int s1(int x)
{
  switch (x)
    {
    case 10:
    case 11:
      return 2;
    case 12:
      return 123;
    default:
      return x + 2;
    }
}

__attribute__ ((noinline))
int s2(int x)
{
  switch (x)
    {
    case 10:
    case 11:
      return 2;
    case 12:
      return 123;
    default:
      return x + 2;
    }
}
int main(int argc, char **argv)
{
  if(argc < 3)
    return 1;

  int a = atoi(argv[1]);
  int b = atoi(argv[2]);

  printf("Test1: %d, %d, gdc: %d\n", a, b, gcd(a, b));
  printf("Test2: %d, %d, gdc: %d\n", a, b, nsd(a, b));
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:s1/\[0-9+\]+->s2/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:nsd_different_result/\[0-9+\]+->nsd_different_result2/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:gcd/\[0-9+\]+->nsd/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 3" "icf"  } } */
