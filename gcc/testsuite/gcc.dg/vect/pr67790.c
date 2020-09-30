/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

struct {
    int x_advance;
    int y_advance;
} a[256];

int b, c;

void __attribute__((noinline,noclone)) fn1()
{
  for (int i = 0; i < 256; i++)
    {
      c -= a[i].x_advance;
      b -= a[i].y_advance;
    }
}

int main()
{
  check_vect ();

  for (int i = 0; i < 256; ++i)
    {
      a[i].x_advance = i;
      a[i].y_advance = -i + 3;
      __asm__ volatile ("" : : : "memory");
    }
  
  fn1();

  if (c != -32640 || b != 31872)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
