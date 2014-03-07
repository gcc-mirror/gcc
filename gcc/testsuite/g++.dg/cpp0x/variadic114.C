// PR c++/49785
// { dg-do compile { target c++11 } }

template <typename, typename ...> struct B { };
template <typename> class A;

template <typename R, typename ... S>
struct A <R (S ...)> : public B <R, S ...>
{
  struct C {};
  template <typename D> A (D, C = C ()) { }
  R operator () (...);
};

template <typename R, typename ... S, typename T>
auto operator >> (A <R (S ...)>, T)->A <R (S ...)>
{
  []() {};
}

int
main ()
{
  A <int (int, int)> a = [](int, int) {};
  auto b = []{};
  (a >> b) (3, 5);
}
