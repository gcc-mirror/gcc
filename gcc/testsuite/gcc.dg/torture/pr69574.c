/* { dg-do compile } */

typedef unsigned mytype;

struct S {
    mytype *pu;
};

mytype f(struct S *e)
{
  mytype x;
  if(&x != e->pu)
    __builtin_memcpy(&x, e->pu, sizeof(unsigned));
  return x;
}
