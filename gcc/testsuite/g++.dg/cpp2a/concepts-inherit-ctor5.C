// { dg-do compile { target concepts } }

template <class T> struct A
{
  constexpr A(T) requires (sizeof(T) > 1) {}

  A(T);
};

template <class T> struct B: A<T>
{
  using A<T>::A;
};

int main()
{
  constexpr B<int> b = 42;
}
