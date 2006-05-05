void foo()
{
  struct A a;  /* { dg-error "storage size" } */
  a.i;
}
