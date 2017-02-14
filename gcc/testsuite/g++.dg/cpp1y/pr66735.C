// { dg-do compile { target c++14 } }

// PR c++/66735, lost constness on reference capture

template <typename T> void Foo ()
{
  T const x = 5;

  auto l = [&rx = x]() {};

  l ();
}

void Baz ()
{
  int const x = 5;
  auto l = [&rx = x]() {};


  l ();
  Foo<int> ();
}
