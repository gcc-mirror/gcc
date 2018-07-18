// PR c++/80891 part 3
// We were failing to mark OVERLOADS held in template definitions as
// immutable in non-call contexts.

namespace std {
  int endl();
}

using std::endl;

template <class RealType> void test_spots(RealType)
{
  using namespace std;
  RealType a;
  a << endl;
}

template <typename T>
void operator<< (T, int (&)());

struct Q {};
void test_maintest_method()
{
  Q q;
  test_spots(q);
}
