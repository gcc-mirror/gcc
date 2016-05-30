// PR c++/59366
// { dg-do compile }
template<typename T> void f(T);

struct S
{
  template<typename T> friend void f(T) {}
  template<typename T> friend void g(T) {}
  template<typename T> friend void h(T) {}
};

template<typename T> void h(T);

int
main ()
{
  f(1);
  g(1); // { dg-error "3:'g' was not declared in this scope" }
  g(S());
  h(1);
}
