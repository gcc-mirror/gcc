// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wshadow" }

// pr67273 bogus warning about shadowing.


template <typename T> void Foo (T &&lambda)
{
  int ARG = 2;
  lambda (1);
}

void Baz ()
{
  Foo ([] (auto &&ARG) {});
}
