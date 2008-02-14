// PR c++/11159 : erroneous warning in copy ctor with virtual inheritance
// { dg-do compile }
// { dg-options "-Wall -Wextra" }
struct A
{
  A ();
};

struct B : virtual A
{
  B ();
};

struct C : virtual A
{
  C ();
};

struct D : B, C
{
  D (D const&){}
};

template <typename Base>
struct E : Base
{
  E ();

  E (E const &)
    : Base ()
  {
  };
};

E<C> foo;
E<C> bar (foo);

