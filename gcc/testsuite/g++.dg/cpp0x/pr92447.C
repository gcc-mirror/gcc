// PR c++/92447
// { dg-do compile { target c++11 } }

template <typename T>
void
foo ()
{
  struct S { S &operator=(S &&x) = default; const T s{}; };
}

void bar ()
{
  foo<int>();
}
