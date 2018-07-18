// PR c++/78649
// { dg-do compile { target c++11 } }

template <class> void foo ();
template <class T, class... U>
void
test ()
{
  T t (foo<U>...);	// { dg-error "declared void" }
}

int
main ()
{
  test<void> ();
}
