// { dg-do assemble  }
// Bug: g++ generates code for assignment in invalid situations.

class X {
  int& a;
public:
  X(int& i): a(i) { };
};

void foo ()
{
  int one=1, two=2;
  X a(one), b(two);
  a = b;			// { dg-error "" } no assignment semantics defined
}
