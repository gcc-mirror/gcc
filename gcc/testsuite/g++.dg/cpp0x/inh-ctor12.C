// { dg-do run { target c++11 } }

struct A
{
  int i;
  template <class T>
  A(T t) noexcept : i(t) {}
};

struct C
{
  C() { throw 42; }
};

struct B: A, C
{
  using A::A;
};

int main()
{
  try { B b(24); }
  catch (int) { return 0; }
  __builtin_abort();
}
