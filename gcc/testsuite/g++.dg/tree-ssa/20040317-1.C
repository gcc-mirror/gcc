/* { dg-do run } */
/* { dg-options "-O2" } */

/* Test provided by Brian Ryner in PR 14511.  The alias analyzer was
   not handling structures containing arrays properly.  In this case,
   the static cast was introducing two assignments of the form

	  this_6->_vptr.IFoo = &_ZTV4IFoo[2];
	  this_4->_vptr.IFoo = &_ZTV3Bar[2];

   which were not considered to alias each other because the alias
   analyzer was not computing a proper pointer to array elements.
   Another related bug was the type based alias analyzer not computing
   alias relations to _ZTV4IFoo and _ZTV3Bar.  Since those variables
   are read-only, it was disregarding alias information for them.
   So, the memory tags for the two 'this' variables were not being
   marked as aliased with these variables.  Resulting in the two
   assignments not aliasing each other.

   This was causing the optimizers to generate a call to the virtual
   method Foo() instead of the overloaded version.  */

struct IFoo
{
  virtual void Foo() = 0;
};

struct Bar : IFoo
{
  void Foo() { }
};

int main(int argc, char **argv)
{
  Bar* b = new Bar();
  static_cast<IFoo*>(b)->Foo();
  return 0;
}
