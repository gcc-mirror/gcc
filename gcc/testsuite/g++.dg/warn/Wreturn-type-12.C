// PR c++/94742
// { dg-do compile { target c++11 } }
// { dg-options "-Wreturn-type" }

template <class T>
[[noreturn]] void
foo (T const &t, char const *)
{
  throw T (t);
}

template <class U>
int
bar ()
{
  foo (42, __FUNCTION__);
}	// { dg-bogus "no return statement in function returning non-void" }

int
main ()
{
  bar<long>();
}
