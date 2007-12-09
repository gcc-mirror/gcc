// PR c++/34178
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }

template<typename T>
class A
{
private:
  static const int x;
  static int y;

public:
  int getX () { return x + y; }
};

template<typename T> const int A<T>::x = 0;
template<typename T> int A<T>::y = 0;

int
main ()
{
  A<int> a;
  return a.getX();
}
