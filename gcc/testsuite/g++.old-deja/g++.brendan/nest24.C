// { dg-do assemble  }
// GROUPS passed nested-classes
struct A {
  A (){}
};

void foo ()
{
 struct B {};

 struct S : B {
   A a;
 };
}
