/* { dg-do compile } */
/* { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef unsigned mytype;

struct S {
    mytype *pu;
};

mytype f(struct S *e)
{
  mytype x;  /* { dg-bogus { "uninitialized" } } */
  if(&x != e->pu)
    __builtin_memcpy(&x, e->pu, sizeof(unsigned));
  return x;
}

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
