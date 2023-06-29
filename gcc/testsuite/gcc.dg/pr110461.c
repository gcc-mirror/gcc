/* { dg-do compile } */
/* { dg-options "-O" } */

typedef int v4si __attribute__ ((vector_size (4*sizeof(int))));
typedef short v4hi __attribute__ ((vector_size (4*sizeof(short))));

v4hi res;
v4hi a, b;

void f(void)
{
  v4si t = __builtin_convertvector (a, v4si);
  v4si t1 = __builtin_convertvector (b, v4si);
  t ^= t1;
  res = __builtin_convertvector (t, v4hi);
}
