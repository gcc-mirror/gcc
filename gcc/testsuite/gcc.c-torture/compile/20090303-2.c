/* The array offset became 0x1ffffffffffffffe via a conversion from
   signed to unsigned HOST_WIDE_INT, causing an ICE compiling for
   Thumb.  */

int r (unsigned short *);
void s (unsigned short *, unsigned short *);

int
f (int x)
{
  unsigned short a[1], c[1];

  if (r (a))
    return x;

  if (c[0x7fffffff])
    s (a, c);

  return 0;
}
