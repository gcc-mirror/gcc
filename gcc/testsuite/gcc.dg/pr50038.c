/* PR target/50038 */
/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-O2" } */

void pr50038(int len, unsigned char *in, unsigned char *out)
{
  int i;
  unsigned char xr, xg;
  unsigned char xy=0;
  for (i = 0; i < len; i++)
    {
      xr = *in++;
      xg = *in++;
      xy = (unsigned char) ((19595*xr + 38470*xg) >> 16);

      *out++ = xy;
    }
}

/* { dg-final { scan-assembler-times "movzbl" 2 } } */
