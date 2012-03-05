/* PR rtl-optimization/52113 */

unsigned long v1;
unsigned char v2;

void
foo (void)
{
  if (v1 > (v2 * 1000L))
    v1 = 0;
}
