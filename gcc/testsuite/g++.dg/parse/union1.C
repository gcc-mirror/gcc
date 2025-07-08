// PR c++/83469
// { dg-do compile }

struct S {
  union U { int m; };
};

template <typename T>
void
f ()
{
  union T::U u;
}

int
main()
{
  f<S>();
}
