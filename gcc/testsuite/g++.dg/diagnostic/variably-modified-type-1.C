// { dg-options "" }

void foo ()
{
  int n;
  typedef int X[n];
  struct Z
  {
    X x __attribute__((unused)); // { dg-error "7:data member may not have variably modified type" }
    void bar (X x __attribute__((unused))); // { dg-error "17:parameter may not have variably modified type" }
  };
}
