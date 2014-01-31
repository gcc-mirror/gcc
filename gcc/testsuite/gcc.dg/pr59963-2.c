/* PR c/59963 */
/* { dg-do compile } */
/* { dg-options "-Woverflow -Wconversion" } */

extern void bar (unsigned char);
extern void bar8 (unsigned char, unsigned char, unsigned char, unsigned char,
		  unsigned char, unsigned char, unsigned char, unsigned char);
extern void bazu (unsigned int, ...);
extern void bazi (char, int);
extern int f (short a, short b);

int
g (void)
{
  return f (0xffffffffL, /* { dg-warning "13:overflow in implicit constant conversion" } */
            0xffffffffL) /* { dg-warning "13:overflow in implicit constant conversion" } */
	 && f (0xffffffffL, /* { dg-warning "9:overflow in implicit constant conversion" } */
	       0xffffffffL); /* { dg-warning "9:overflow in implicit constant conversion" } */
}

void
foo (int i)
{
  bar (256); /* { dg-warning "8:large integer implicitly truncated to unsigned type" } */
  bar (6.66f); /* { dg-warning "8:conversion" } */
  bar8 (-1, /* { dg-warning "9:negative integer implicitly converted to unsigned type" } */
	 -2, /* { dg-warning "3:negative integer implicitly converted to unsigned type" } */
	  -3, /* { dg-warning "4:negative integer implicitly converted to unsigned type" } */
	   -4, /* { dg-warning "5:negative integer implicitly converted to unsigned type" } */
	    -5, /* { dg-warning "6:negative integer implicitly converted to unsigned type" } */
	     -6, /* { dg-warning "7:negative integer implicitly converted to unsigned type" } */
	      -7, /* { dg-warning "8:negative integer implicitly converted to unsigned type" } */
	       -8); /* { dg-warning "9:negative integer implicitly converted to unsigned type" } */
  bazu (i, i); /* { dg-warning "9:conversion" } */
  bazi (0x8, 0x80000000); /* { dg-warning "14:conversion of unsigned constant value to negative integer" } */
}
