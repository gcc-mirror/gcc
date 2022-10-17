/* { dg-do compile } */
/* { dg-options "-O3" } */

unsigned __attribute__ ((noinline))
foo (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  while (n < ++l)
    *a++ = *b++ + 1;
  return l;
}

volatile int a[1];
unsigned b;
int c;

int
check ()
{
  int d;
  for (; b > 1; b++)
    for (c = 0; c < 2; c++)
      for (d = 0; d < 2; d++)
	a[0];
  return 0;
}

char **clip_image_gfi_0;
int clip_image_y, clip_image_shift;
void
clip_image ()
{
  for (; clip_image_y >= clip_image_shift; clip_image_y++)
    clip_image_gfi_0[clip_image_shift]
      = clip_image_gfi_0[clip_image_y];
}
