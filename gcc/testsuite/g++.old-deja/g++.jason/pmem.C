// { dg-do assemble  }
// Bug: g++ parses the declaration of 'char A::* foo' below as a
// declaration of 'char A'.

class A { };
typedef int foo;
void f ()
{
  char A::* foo;
  foo = 0;			// { dg-bogus "" } parsing blunder
}
