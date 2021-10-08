// PR c++/102496
// { dg-do link { target c++11 } }
// { dg-require-effective-target tls }
// { dg-add-options tls }
// { dg-additional-sources pr102496-2.C }

template <int N>
int
foo ()
{
  extern __thread int t1;
  return t1;
}

int
main ()
{
  extern __thread int t2;
  return foo <0> () + t2;
}
