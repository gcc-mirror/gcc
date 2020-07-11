// P2002: Allow default comparison of unions with no members.
// { dg-do compile { target c++20 } }

union A
{
  bool operator==(const A&) const = default;
};

int main()
{
  A() == A();
}
