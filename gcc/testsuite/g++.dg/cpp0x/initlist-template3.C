// PR c++/97899
// { dg-do compile { target c++11 } }

template <typename T = int>
int fn()
{
  return 1;
}

template <typename T>
void bar() {
  const int i = int{fn()};
}
