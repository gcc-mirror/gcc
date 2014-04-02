// Test that we properly value-initialize a class with a user-provided
// constructor but defaulted default constructor.  The FDIS got this
// wrong; see c++std-core-19883.

// { dg-do run { target c++11 } }

struct A
{
  int i;
  A() = default;
  A(int);
};

int main()
{
  A a{};
  if (a.i != 0)
    return 1;
}
