// Build don't link: 

// this is marked as an expected error because it evidences an
// ambiguity in the grammar between expressions and declarations.
// when the parser's been cleaned up or rewritten, the error
// marker can go away, since it'll no longer occur.

class A { };

int main()  {
  A a = a;
  A b(b); // gets bogus error - XFAIL *-*-*
}
