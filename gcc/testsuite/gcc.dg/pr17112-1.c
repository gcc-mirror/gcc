/* PR middle-end/17112 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

typedef struct {
  int int24:24  __attribute__ ((packed)); /* { dg-warning "attribute ignored" "" { target { default_packed && { ! pcc_bitfield_type_matters } } } } */
} myint24;

myint24 x[3] = {
  0x123456,
  0x789abc,
  0xdef012
};

myint24 y[3];  // starts out as zeros

void foo()
{
  y[1] = x[1];
}

int main()
{
  foo();

  if (y[0].int24 != 0 || y[2].int24 != 0)
    abort();
  return 0;
}

