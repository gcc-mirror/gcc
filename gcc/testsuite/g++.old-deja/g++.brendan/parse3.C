// Build don't link: 
// crash test

// these are marked as expected errors because they evidence an
// ambiguity in the grammar between expressions and declarations.
// when the parser's been cleaned up or rewritten, these two error
// markers can go away, since they'll no longer occur.

class A
{
  public:
    int high;
    unsigned int low;
    A operator+(const A in);
};

A A::operator+(const A in)
{
    if (high==0)
      return A();    // this works
    else
      return (A());  // this works not // gets bogus error - XFAIL *-*-* 
}
