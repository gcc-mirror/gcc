// { dg-do compile }

struct H { typedef int type; };
struct J : H
{
  struct type {}; // { dg-message "previous" }
  using H::type; // { dg-error "conflicts" }
};
