/* { dg-do compile } */
int f(int i)
{
  static int g(); /* { dg-warning "invalid storage class" } */
  static int g() { return i; } /* { dg-warning "invalid storage class" } */
  return g();
}
