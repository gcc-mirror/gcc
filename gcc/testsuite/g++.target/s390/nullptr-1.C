// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wpsabi" }

// Ensure that after template instantiation no note is emitted.

template <typename T, typename FuncT>
static __attribute__ ((noinline)) T
call (FuncT &&F)
{
  return F();
}

int *
test ()
{
  return call<int *> ([] () { return nullptr; });
}
