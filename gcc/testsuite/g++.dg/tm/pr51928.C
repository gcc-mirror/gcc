/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

struct A; // { dg-error "forward declaration of 'struct A'" }

struct B
{
  virtual B* foo(A);
};

struct C : virtual B
{
  virtual C* foo(A) { return 0; } // { dg-error "'<anonymous>' has incomplete type" }
};

C c;
