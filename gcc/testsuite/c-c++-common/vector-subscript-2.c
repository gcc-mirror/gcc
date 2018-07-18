/* { dg-do compile } */

/* Check that subscripting of vectors work with register storage class decls.  */

#define vector __attribute__((vector_size(16) ))


float vf(int i)
{
  register vector float a;	// { dg-warning "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
  return a[0];
}
