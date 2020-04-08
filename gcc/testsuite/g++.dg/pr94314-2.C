/* PR c++/94314.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-cddce-details" } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */

#include <stdio.h>

struct A
{
  __attribute__((always_inline)) A(int x)
  {
    if (x == 123)
      throw x;
  }
};

int
main(int argc, char **argv)
{
  A *a = new A (argc);
  delete a;

  return 0;
}

/* { dg-final { scan-tree-dump-times "Deleting : operator delete" 2 "cddce1"} } */
