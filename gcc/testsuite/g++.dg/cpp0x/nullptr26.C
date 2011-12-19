// PR c++/51530
// { dg-options -std=c++0x }

template <class T, class U>
void f(T, U);

template <class T>
void f(T, decltype(nullptr));

int main()
{
  f(1, nullptr);
}
