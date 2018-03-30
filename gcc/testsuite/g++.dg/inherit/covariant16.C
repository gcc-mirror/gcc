/* PR c++/28253 This used to ICE. */
/* { dg-do compile } */

struct A
{
  virtual A* foo();
};

struct B : virtual A
{
  virtual B* foo(); /* { dg-message "overridden" } */
};

struct C : B
{
  virtual C& foo(); /* { dg-error "conflicting return type" } */
};
