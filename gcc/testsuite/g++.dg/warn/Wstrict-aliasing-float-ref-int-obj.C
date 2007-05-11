/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo() {
  int x;
  float& q = reinterpret_cast<float&> (x);  /* { dg-warning "type-punn" } */
  q = 1.0;
  return x;
}
