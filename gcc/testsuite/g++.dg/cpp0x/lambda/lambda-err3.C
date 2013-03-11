// PR c++/51412
// { dg-do compile { target c++11 } }

void foo(int);

template<int> void bar()
{
  foo([]{}...);      // { dg-error "<lambda>" }
  foo([]{}=0 ...);   // { dg-error "<lambda>" }
}
