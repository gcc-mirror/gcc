// PR c++/22318. Improve diagnostic for local template declaration.
// { dg-do compile }
void f(void)
{
  template<typename T> class A /* { dg-error "a template declaration cannot appear at block scope" } */
  {
  };
}

void g(void)
{
  template f<int>(); /* { dg-error "expected primary-expression" } */
  /* { dg-error "expected ';'" "" { target *-*-* } 12 } */
}
