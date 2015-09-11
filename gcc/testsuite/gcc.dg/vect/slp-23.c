/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   int a;
   int b;
   int c;
   int d;
   int e;
   int f;
   int g;
   int h;
} s;

int
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];

  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->c + ptr->c;
      res[i].a = ptr->a + ptr->a;
      res[i].d = ptr->d + ptr->d;
      res[i].b = ptr->b + ptr->b;
      res[i].f = ptr->f + ptr->f;
      res[i].e = ptr->e + ptr->e; 
      res[i].h = ptr->h + ptr->h;   
      res[i].g = ptr->g + ptr->g;
      ptr++; 
    } 
   
  /* check results:  */
  for (i = 0; i < N; i++)
    { 
      if (res[i].c != arr[i].c + arr[i].c
          || res[i].a != arr[i].a + arr[i].a
          || res[i].d != arr[i].d + arr[i].d
          || res[i].b != arr[i].b + arr[i].b
          || res[i].f != arr[i].f + arr[i].f
          || res[i].e != arr[i].e + arr[i].e
          || res[i].h != arr[i].h + arr[i].h
          || res[i].g != arr[i].g + arr[i].g)
         abort();
    }

  ptr = arr;
  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->c + ptr->c;
      res[i].a = ptr->a + ptr->a;
      res[i].d = ptr->d + ptr->d;
      res[i].b = ptr->b + ptr->b;
      res[i].f = ptr->f + ptr->f;
      res[i].e = ptr->e + ptr->e;
      res[i].h = ptr->e + ptr->e;
      res[i].g = ptr->g + ptr->g;
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].c != arr[i].c + arr[i].c
          || res[i].a != arr[i].a + arr[i].a
          || res[i].d != arr[i].d + arr[i].d
          || res[i].b != arr[i].b + arr[i].b
          || res[i].f != arr[i].f + arr[i].f
          || res[i].e != arr[i].e + arr[i].e
          || res[i].h != arr[i].e + arr[i].e
          || res[i].g != arr[i].g + arr[i].g)
         abort();
    }

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
      arr[i].c = 17;
      arr[i].d = i+34;
      arr[i].e = i * 3 + 5;
      arr[i].f = i * 5;
      arr[i].g = i - 3;
      arr[i].h = 56;
      if (arr[i].a == 178)
         abort(); 
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target { vect_strided8 && { ! { vect_no_align} } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! { vect_strided8 || vect_no_align } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { ! vect_perm } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_perm } } } */
  
