// { dg-do compile { target c++20 } }
template <typename t>
struct S
{
  template <typename t2>
    requires false
  friend void foobar(S, t2) {}
};

int main()
{
  foobar(S<double>{}, int{}); // { dg-error "" }
}
