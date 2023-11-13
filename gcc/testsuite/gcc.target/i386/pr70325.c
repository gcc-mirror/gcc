/* PR target/70325 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -mavx512vl -O2" } */

typedef char C __attribute((__vector_size__(32)));
typedef int I __attribute((__vector_size__(32)));

void
f(int a,I b)
{
  __builtin_ia32_storedquqi256_mask((C*)f,(C)b,a); /* { dg-warning "implicit declaration of function" } */
}
