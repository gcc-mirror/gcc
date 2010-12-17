/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  int b;
}str_t;

#define N 3

int
main ()
{
  int i;
  int res = 1<<(1<<N);
  str_t str;
  
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
/* { dg-final { scan-ipa-dump "No structures to transform" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
