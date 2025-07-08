// PR c++/83469
// { dg-do compile }

struct S {
  struct C { int m; };
};

template <typename T>
void
f ()
{
  union T::C u; // { dg-error "not a union type" }
}

int
main()
{
  f<S>();
}
