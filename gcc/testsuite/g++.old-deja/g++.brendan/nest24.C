// Build don't link: 
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
