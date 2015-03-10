// PR c++/65127
// { dg-do compile { target c++11 } }

template <int N>
void
foo ()
{
  static int i {100};
  struct { int id {i++}; } j;
}

int
main ()
{
  foo<0> ();
}
