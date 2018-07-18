// PR c++/72775
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct S {
  int i;
  char a[] = "foo";   // { dg-error "initializer for flexible array member" }
  S () {}
};

struct T {
  int i;
  char a[] = "foo";   // { dg-error "initializer for flexible array member" }
};

struct U {
  int i;
  char a[] = "foo";   // { dg-error "initializer for flexible array member" }
  U ();
};

U::U() {}

int
main ()
{
  struct T t;
}

struct V {
  int i;
  struct W {
    int j;
    char a[] = "foo";   // { dg-error "initializer for flexible array member" }
  } w;
  V () {}
};

template <class T>
struct X {
  int i;
  T a[] = "foo";   // { dg-error "initializer for flexible array member" }
};

void
fn ()
{
  struct X<char> x;
}
