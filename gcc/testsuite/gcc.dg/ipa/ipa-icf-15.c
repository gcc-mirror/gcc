/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

#include <stdio.h>

__attribute__ ((noinline))
int bar(int a)
{
  void *l = &&error;

  if(a == 4)
    goto *l;

  return 150;

error:
  return a;
failure:
  return a + 2;
}

__attribute__ ((noinline))
int foo(int a)
{
  void *l = &&error;

  if(a == 4)
    goto *l;

  return 150;

error:
  return a;
failure:
  return a + 2;
}

int main(int argc, char **argv)
{
  printf("value: %d\n", foo(argc));

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
