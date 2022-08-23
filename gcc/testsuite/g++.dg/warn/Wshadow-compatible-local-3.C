// PR c++/100608
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wshadow=compatible-local" }

template <typename> class X {};

void foo()
{
  auto a = X<class a>{};	// no warning, not compatible
}
