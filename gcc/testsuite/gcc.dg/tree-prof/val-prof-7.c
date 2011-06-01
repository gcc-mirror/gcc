/* { dg-options "-O2 -fdump-ipa-profile -mtune=core2" } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */

#include <strings.h>

int foo(int len)
{
  char array[1000];
  bzero(array, len);
  return 0;
}

int main() {
  int i;
  for (i = 0; i < 1000; i++)
    {
      if (i > 990)
	foo(16);
      else
	foo(8);
    }
  return 0;
}

/* { dg-final-use { scan-ipa-dump "Single value 8 stringop transformation on bzero" "profile" } } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
