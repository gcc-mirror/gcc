// PR c++/45332
// { dg-do compile }

class C
{
 int x				// { dg-error "at end of member declaration" }

 const int foo() { return x; }
};
