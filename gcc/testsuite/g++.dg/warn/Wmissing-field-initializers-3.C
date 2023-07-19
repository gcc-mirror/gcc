// PR c++/110064
// { dg-do compile { target c++17 } }
// { dg-options "-Wmissing-field-initializers" }

struct B { };
struct D : B {
    int x;
    int y;
};

struct E {
  int x;
  int y;
  B z;
};

template<typename> struct X { };

template<typename T>
struct F {
  int i;
  int j;
  X<T> x;
};

int
main ()
{
  D d = {.x=1, .y=2}; // { dg-bogus "missing" }
  (void)d;
  E e = {.x=1, .y=2}; // { dg-bogus "missing" }
  (void)e;
  F<int> f = {.i=1, .j=2 }; // { dg-bogus "missing" }
  (void)f;
}

template<typename T>
void fn ()
{
  F<T> f = {.i=1, .j=2 }; // { dg-bogus "missing" }
  (void)f;
}

void
g ()
{
  fn<int> ();
}
