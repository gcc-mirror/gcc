/* PR debug/47881 */
/* { dg-do compile } */
/* { dg-options "-O -fcompare-debug -fno-dce -funroll-loops -fno-web" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

extern int data[];

int
foo (int *t, int *f, int n)
{
  int i = 0, a, b, c, d;
  while (data[*f] && n)
    n--;
  for (; i < n; i += 4)
    {
      a = data[*(f++) & 0x7f];
      c = data[*(f++) & 0x7f];
      c = data[*(f++) & 0x7f];
      d = data[*(f++) & 0x7f];
      if ((a & 0x80) || (b & 0x80) || (c & 0x80) || (d & 0x80))
	return 1;
      *(t++) = 16;
    }
  return 0;
}
