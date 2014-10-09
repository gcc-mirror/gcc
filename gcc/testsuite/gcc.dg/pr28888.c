/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

static const unsigned char _c30[] =
  "statistic of allocated integer registers:";
void Out__LongInt ();
int
Allocate__WriteStats (void)
{
  register int i0, i1, i2, i3, i4, i5;
l0:i1 = (int) (i5 << 2);
  if (i0)
  i4 = i5;
l1:i2 += i1;
  if (i1)
  goto l0;
l3:i0 = i1 == 255;
  i1++;
  Out__LongInt ((int) i0, (int) 0);
  i0 = i4 >= i1;
  if (i0)
    goto l3;
}
