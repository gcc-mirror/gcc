// PR c++/78244
// { dg-do compile { target c++11 } }

using Number = unsigned int;

template <int>
struct S {
  S() {
    const Number x = {-1}; // { dg-error "narrowing conversion" }
    (void)x;
  }
};

int main()
{
  S<1> s;
}
