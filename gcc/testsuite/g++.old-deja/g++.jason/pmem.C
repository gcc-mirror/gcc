// Bug: g++ parses the declaration of 'char A::* foo' below as a
// declaration of 'char A'.
// Build don't link:

class A { };
typedef int foo;
void f ()
{
  char A::* foo;
  foo = 0;			// gets bogus error - parsing blunder
}
