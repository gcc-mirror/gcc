// P0634R3, PR c++/88358
// { dg-do compile { target c++2a } }

template <typename T>
int pi(T::your_pi);

struct Foo { static constexpr int your_pi = 10; };

int
main ()
{
  return pi<Foo>;
}
