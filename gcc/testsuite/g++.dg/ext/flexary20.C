// PR c++/72775
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct S {
  int i;
  char a[] = "foo";
  S () {} // { dg-error "member initializer for flexible array member" }
};

struct T { // { dg-error "member initializer for flexible array member" }
  int i;
  char a[] = "foo";
};

struct U {
  int i;
  char a[] = "foo";
  U ();
};

U::U() {} // { dg-error "member initializer for flexible array member" }

int
main ()
{
  struct T t;
}

struct V {
  int i;
  struct W { // { dg-error "member initializer for flexible array member" }
    int j;
    char a[] = "foo";
  } w;
  V () {}
};

template <class T>
struct X { // { dg-error "member initializer for flexible array member" }
  int i;
  T a[] = "foo";
};

void
fn ()
{
  struct X<char> x;
}
