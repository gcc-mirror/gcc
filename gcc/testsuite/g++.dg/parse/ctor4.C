// PR c++/27279
// { dg-do compile }

struct A
{
  A(void,void);  // { dg-error "incomplete type|invalid use" }
};
