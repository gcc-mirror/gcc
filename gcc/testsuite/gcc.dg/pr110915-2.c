/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-psabi" } */

#include <limits.h>

#define vector __attribute__((vector_size(sizeof(unsigned)*2)))

vector signed and1(vector unsigned x, vector unsigned y)
{
  /* (x > y)   &   (x != 0)  --> x > y */
  return (x > y)   &   (x != 0);
}

vector signed and2(vector unsigned x, vector unsigned y)
{
  /* (x < y)   &   (x != UINT_MAX)  --> x < y */
  return (x < y)   &   (x != UINT_MAX);
}

vector signed and3(vector signed x, vector signed y)
{
  /* (x > y)   &   (x != INT_MIN)  --> x > y */
  return (x > y)   &   (x != INT_MIN);
}

vector signed and4(vector signed x, vector signed y)
{
  /* (x < y)   &   (x != INT_MAX)  --> x < y */
  return (x < y)   &   (x != INT_MAX);
}

/* { dg-final { scan-tree-dump-not " != " "optimized" } } */
