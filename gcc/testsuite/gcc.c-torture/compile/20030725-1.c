/* This testcase caused ICE on any 64-bit arch at -O2/-O3 due to
   fold/extract_muldiv/convert destroying its argument.  */
int x, *y, z, *p;

void
foo (void)
{
  p = y + (8 * (x == 1 || x == 3) + z);
}
