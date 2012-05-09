/* Check the case when index is out of bound */
/* { dg-do compile } */
/* { dg-options "-Warray-bounds" } */

#define vector __attribute__((vector_size(16) ))


int test0(void)
{
  vector int a;
  return a[10]; /* { dg-warning "index value is out of bound" } */
}

int test1(void)
{
  vector int a;
  return a[-1]; /* { dg-warning "index value is out of bound" } */
}
