/* PR102281  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -Wno-psabi" } */
long long var1;
float var2;
typedef long long V __attribute__((__vector_size__(2 * sizeof(long long))));
typedef float W __attribute__((__vector_size__(4 * sizeof(float)))); 

V foo (void)
{
  return (V) {var1}; 
}

W bar (void)
{
  return (W) {var2};
}
