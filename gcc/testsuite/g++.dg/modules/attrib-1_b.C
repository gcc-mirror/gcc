// { dg-additional-options -fmodules }

template <class T> struct A;

import M;

int main()
{
  a<int>.f();
}
