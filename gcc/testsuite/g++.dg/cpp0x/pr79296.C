// { dg-do compile { target c++11 } }
// { dg-require-effective-target lto }
// { dg-additional-options "-flto" }

// PR 79296 ICE mangling local class of localized instantiation

struct X {
  template <typename T> X (T const *) {
    struct Z {};
  }
};

void Baz ()
{
  struct Y { } y;

  0, X (&y);
}
