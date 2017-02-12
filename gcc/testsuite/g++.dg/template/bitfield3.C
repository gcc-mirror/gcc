// PR c++/78908

struct A { int a : 1; };
struct F { int foo (A const &); };
template <typename> struct O : F { int foo (A const &); };
struct S {} b;
template <typename L, typename T> int operator<< (L, T) { return (T) 123; }
template <typename T> int O<T>::foo (A const &x) { return b << x.a; }

int
main ()
{
  A a = { 0 };
  O<int> o;
  if (o.foo (a) != 123)
    __builtin_abort ();
  signed char d = 2;
  if ((b << d) != 123)
    __builtin_abort ();
}
