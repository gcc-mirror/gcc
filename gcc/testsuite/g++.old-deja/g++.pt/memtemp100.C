// Build don't link:
// Origin: philippeb@videotron.ca
// Special g++ Options:

#include <iostream>

using namespace std;

template <class T> struct traits
{
  typedef long next;
};


template <class T>
struct c1
{
  template <class U>
  struct c2
  {
    c2()
    {
      cout << __PRETTY_FUNCTION__ << endl;
    }
  };
};


template <class T>
void foo()
{
  cout << __PRETTY_FUNCTION__ << endl;
  typename c1<typename traits<T>::next>::template c2<void>();
}


int main()
{
  foo<int>();
}
