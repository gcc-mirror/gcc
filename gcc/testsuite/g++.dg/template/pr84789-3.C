// { dg-do compile }

struct A
{
  static int i;
};
struct B
{
  typedef ::A A;
};
int B::A::i = 0;

struct K
{
  struct L
  {
    template <typename T>
    static void f(T);
  };
};

template <typename T>
struct O
{
  typedef K Q;
};

template <typename T>
void O<T>::Q::L::f(T)
{
}
