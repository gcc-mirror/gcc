// { dg-do compile }
// PR c++/48010

struct A
{
  struct type {}; // { dg-message "previous" }
  typedef int type; // { dg-error "conflicts" }
};
