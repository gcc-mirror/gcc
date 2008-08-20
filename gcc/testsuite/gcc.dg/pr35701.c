/* PR 35701 */
/* { dg-do compile } */
/* { dg-options "-Wconversion -Wsign-conversion" } */
/* { dg-require-effective-target int32plus } */

typedef struct _my_struct_t {
  unsigned int small:1;
  unsigned int big:31;
} my_struct_t, *my_struct_p_t;

void
my_func1(unsigned int sm, unsigned int bi, my_struct_p_t msp)
{
  msp->small = sm; /* { dg-warning "conversion" } */
  msp->big = bi; /* { dg-warning "conversion" } */
}

void
my_func2(unsigned int sm, unsigned int bi, my_struct_p_t msp)
{
  msp->small = sm & 1U;
  msp->big = bi & 0x7fffffffU;
}

unsigned short
my_func3(unsigned int sm)
{
  unsigned short res;
  res = sm & 0xff20U;
  return res;
}
