/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  int b;
}str_t;

#define N 3

str_t str;

int
main ()
{
  int i;
  int res = 1<<(1<<N);
  str.a = 2;

  for (i = 0; i < N; i++)
    str.a = str.a * str.a;
  
  if (str.a != res)
    abort ();

  /* POSIX ignores all but the 8 low-order bits, but other
     environments may not.  */
  return (str.a & 255);
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
