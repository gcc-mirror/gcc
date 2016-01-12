/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   int a;
   int b;
   void *c;
} s1;

int
foo1 (s1 *arr)
{
  int i;
  s1 *ptr = arr;

  /* Different constant types - not SLPable.  The group size is not power of 2,
     interleaving is not supported either.  */
  for (i = 0; i < N; i++)
    {
      ptr->a = 6;
      ptr->b = 7;
      ptr->c = 0;
      ptr++; 
    } 
   
  /* check results:  */
  for (i = 0; i < N; i++)
    { 
       if (arr[i].a != 6 
           || arr[i].b != 7
           || arr[i].c != 0)
         abort();
    }
}

int main (void)
{
  int i;
  s1 arr1[N];
  
  check_vect ();

  for (i = 0; i < N; i++)
    { 
      arr1[i].a = i;
      arr1[i].b = i * 2;
      arr1[i].c = (void *)arr1;

      if (arr1[i].a == 178)
         abort(); 
    } 


  foo1 (arr1);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect"  } } */
  
