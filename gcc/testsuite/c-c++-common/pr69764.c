/* PR rtl-optimization/69764 */
/* { dg-do compile { target int32plus } } */

unsigned char
fn1 (unsigned char a)
{
  return a >> ~6;	/* { dg-warning "12:right shift count is negative" } */
}

unsigned short
fn2 (unsigned short a)
{
  return a >> ~6;	/* { dg-warning "12:right shift count is negative" } */
}

unsigned int
fn3 (unsigned int a)
{
  return a >> ~6;	/* { dg-warning "12:right shift count is negative" } */
}

unsigned char
fn4 (unsigned char a)
{
  return a >> 0xff03;	/* { dg-warning "12:right shift count >= width of type" } */
}

unsigned short
fn5 (unsigned short a)
{
  return a >> 0xff03;	/* { dg-warning "12:right shift count >= width of type" } */
}

unsigned int
fn6 (unsigned int a)
{
  return a >> 0xff03;	/* { dg-warning "12:right shift count >= width of type" } */
}
