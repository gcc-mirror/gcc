// Build don't link:
// Origin: philippeb@videotron.ca
// Special g++ Options:

#include <iostream>

using namespace std;

template <class _T> struct traits
{
  typedef long next;
};


template <class _T>
struct c1
{
  template <class _U>
  struct c2
  {
    c2()
    {
      cout << __PRETTY_FUNCTION__ << endl;
    }
  };
};


template <class _T>
void foo()
{
  cout << __PRETTY_FUNCTION__ << endl;
  c1<typename traits<_T>::next>::c2<void>();
}


int main()
{
  foo<int>();
}
