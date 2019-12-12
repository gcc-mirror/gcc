/* [depr.impldec] The implicit definition of a copy constructor as defaulted is
   deprecated if the class has a user-declared copy assignment operator or a
   user-declared destructor. The implicit definition of a copy assignment
   operator as defaulted is deprecated if the class has a user-declared copy
   constructor or a user-declared destructor (15.4, 15.8). In a future revision
   of this International Standard, these implicit definitions could become
   deleted (11.4).  */

// { dg-additional-options -Wdeprecated-copy-dtor }

struct X
{
  X();
  X(const X&);
};
struct A
{
  X x;
  ~A();
};

void f(bool b)
{
  A a;
  if (b)
    throw A();			// Don't warn about elided copy
  A a2 = A();			// Here either.
  A a3 (a);			// { dg-warning "deprecated" "" { target c++11 } }
}
