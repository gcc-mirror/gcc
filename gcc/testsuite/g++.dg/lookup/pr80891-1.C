// PR c++/80891 part 1
// std::endl is found via two paths and most_specialized_instantiation
// gets confused.

namespace std {
  struct A {
    void operator<<(A(A));
  };
  template <typename _CharT, typename _Traits> _CharT endl(_Traits);
  A a;
}

using std::endl;

void chi_squared_sample_sized()
{
  using namespace std;
  a << endl;
}
