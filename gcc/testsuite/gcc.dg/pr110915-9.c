/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine -Wno-psabi" } */

#include <limits.h>

#define vector __attribute__((vector_size(sizeof(unsigned)*2)))

vector signed or1(vector unsigned x, vector unsigned y)
{
  /* (x <= y) | (x != 0)  --> true */
  return (x <= y) | (x != 0);
}

vector signed or2(vector unsigned x, vector unsigned y)
{
  /* (x >= y) | (x != UINT_MAX)  --> true */
  return (x >= y) | (x != UINT_MAX);
}

vector signed or3(vector signed x, vector signed y)
{
  /* (x <= y) | (x != INT_MIN)  --> true */
  return (x <= y) | (x != INT_MIN);
}

vector signed or4(vector signed x, vector signed y)
{
  /* (x >= y) | (x != INT_MAX)  --> true */
  return (x >= y) | (x != INT_MAX);
}

/* { dg-final { scan-tree-dump-not " != " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " <= " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " >= " "ifcombine" } } */
