/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64 

typedef struct {
   unsigned char a;
   unsigned char b;
} s;

__attribute__ ((noinline)) int
main1 (s *arr)
{
  s *ptr = arr;
  s res[N];
  int i;

  for (i = 0; i < N; i++)
    {
      res[i].a = ptr->b;
      res[i].b = ptr->b;
      ptr++;
    }
  
  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].a != arr[i].b
          || res[i].b != arr[i].b)
	abort ();
    }

  ptr = arr;
  /* Not vectorizable: gap in store.  */ 
  for (i = 0; i < N; i++)
    {
      res[i].a = ptr->b;
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].a != arr[i].b)
        abort ();
    }


  return 0;
}

int main (void)
{
  int i;
  s arr[N];
  
  check_vect ();

  for (i = 0; i < N; i++)
    { 
      arr[i].a = i;
      arr[i].b = i * 2;
      if (arr[i].a == 178)
         abort(); 
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target { vect_interleave && vect_extract_even_odd } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
