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
  return f (0xffffffffL, /* { dg-warning "13:-Woverflow" } */
            0xffffffffL) /* { dg-warning "13:-Woverflow" } */
         && f (0xffffffffL, /* { dg-warning "16:-Woverflow" } */
               0xffffffffL); /* { dg-warning "16:-Woverflow" } */
}

void
foo (int i)
{
  bar (256); /* { dg-warning "8:-Woverflow" } */
  bar (6.66f); /* { dg-warning "8:conversion" } */
  bar8 (-1, /* { dg-warning "9:-Wsign-conversion" } */
         -2, /* { dg-warning "10:-Wsign-conversion" } */
          -3, /* { dg-warning "11:-Wsign-conversion" } */
           -4, /* { dg-warning "12:-Wsign-conversion" } */
            -5, /* { dg-warning "13:-Wsign-conversion" } */
             -6, /* { dg-warning "14:-Wsign-conversion" } */
              -7, /* { dg-warning "15:-Wsign-conversion" } */
               -8); /* { dg-warning "16:-Wsign-conversion" } */
  bazu (i, i); /* { dg-warning "9:conversion" } */
  bazi (0x8, 0x80000000); /* { dg-warning "14:-Wsign-conversion" "first" { xfail int16 } } */
			  /* { dg-warning "overflow in conversion from" "second" { target int16 } .-1 } */
}
