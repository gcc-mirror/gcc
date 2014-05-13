// PR c++/54310

template <typename T>
struct meta
{
  typedef typename T::type type;
};

struct S{};

template <typename T>
typename meta<T>::type foo(T, S);

int foo(int, int);      

int main()
{
  foo(0, 0);
}
