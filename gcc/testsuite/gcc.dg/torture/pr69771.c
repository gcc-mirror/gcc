/* PR rtl-optimization/69771 */
/* { dg-do compile } */

unsigned char a = 5, c;
unsigned short b = 0;
unsigned d = 0x76543210;

void
foo (void)
{
  c = d >> ~(a || ~b);	/* { dg-warning "shift count is negative" } */
}
