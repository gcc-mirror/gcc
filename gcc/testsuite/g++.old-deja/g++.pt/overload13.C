// { dg-do assemble  }
struct A {
  template <class T> int f (T) { return 0; }
  int f (int) { return 1; }
};

int main ()
{
  A a;
  return a.template f (0); // { dg-error "template" "" { target { ! c++11 } } }
}
