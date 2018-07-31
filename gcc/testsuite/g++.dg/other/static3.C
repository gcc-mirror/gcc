void foo()
{
  struct S
  {
    static const int a = 0;  // { dg-error "22:local class" }
  };
}
