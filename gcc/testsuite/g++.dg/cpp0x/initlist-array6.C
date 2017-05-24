// PR c++/70167
// { dg-do compile { target c++11 } }

template<class T, unsigned S> void f(T(&&)[S]) { }

using arr = const int[2];

int main()
{
  f(arr{1, 2});
}
