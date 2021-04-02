// PR c++/97187
// { dg-do compile { target c++14 } }
// { dg-options "-fno-exceptions" }

auto yp = [] { return 0; };

template <class DI>
DI
zl ()
{
  auto au = [] () -> DI { return *new auto (true ? yp : throw); }; // { dg-error "exception handling disabled" }

  return au ();
}

auto
vd ()
{
  return zl <decltype (yp)> ();
}
