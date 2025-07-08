// PR c++/83469
// { dg-do compile }

struct S {
  union U { int m; };
};

template <typename T>
void
f ()
{
  struct T::U u;  // { dg-error "not a non-union class type" }
}

int
main()
{
  f<S>();
}
