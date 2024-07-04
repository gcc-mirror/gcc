/* { dg-require-effective-target int32plus } */

#include <stdio.h>

void abort (void);
void exit (int);

typedef struct { int x; int y;} S;
extern int foo(S*);
int hi = 0, lo = 0;

int main()
{
  S a;
  int r;
  a.x = (int) 0x00010000;
  a.y = 1;
  r = foo (&a);
  if (r == 2 && lo==0 && hi==1)
    {
      exit (0);
    }
  abort ();
}

typedef unsigned short u16;

__attribute__ ((noinline)) int foo (S* ptr)
{
  int a = ptr->x;
  int c = 0;
  u16 b = (u16) a;
  if (b != 0)
  {
    lo = 1;
    c += ptr->y;
  }
  b = a >> 16;
  if (b != 0)
  {
    hi = 1;
    c+= ptr->y;
  }
  c += ptr->y;
  return c;
}

     
