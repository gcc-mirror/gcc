// Bug: g++ generates code for assignment in invalid situations.
// Build don't link:

class X {
  int& a;
public:
  X(int& i): a(i) { };
};

void foo ()
{
  int one=1, two=2;
  X a(one), b(two);
  a = b;			// ERROR - no assignment semantics defined
};
