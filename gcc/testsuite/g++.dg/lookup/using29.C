// { dg-do compile }

struct A
{
  int i;
};

struct B
{
  int i;
};

struct C : A, B
{
  using A::i; // { dg-message "previous" }
  using B::i; // { dg-error "redeclaration" }
};

struct E
{
  typedef int type;
};

struct F
{
  typedef int type;
};

struct G : E, F
{
  using E::type; // { dg-message "previous" }
  using F::type; // { dg-error "redeclaration" }
};

struct H
{
  typedef int type;
};

struct I : H
{
  typedef int type; // { dg-message "previous" }
  using H::type; // { dg-error "conflicts" }
};

struct I2 : H
{
  using H::type; // { dg-message "previous" }
  typedef int type; // { dg-error "conflicts" }
};

struct J
{
  struct type {}; // { dg-message "previous" }
};

struct K : J
{
  struct type {}; // { dg-message "previous" }
  using J::type; // { dg-error "conflicts" }
};

struct L : J
{
  using J::type;
  struct type {}; // { dg-error "redefinition" }
};

struct M
{
  typedef int type;
  struct type2 {};
};

struct N : M
{
  using M::type; // { dg-message "previous" }
  using M::type; // { dg-error "redeclaration" }
  using M::type2; // { dg-message "previous" }
  using M::type2; // { dg-error "redeclaration" }
};
