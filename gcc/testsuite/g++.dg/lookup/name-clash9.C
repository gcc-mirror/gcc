// { dg-do compile }
// PR c++/48010

struct A
{
  struct type {}; // { dg-error "conflicts with previous" }
  typedef int type; // { dg-error "declaration" }
};
