/* PR target/96938 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "\tbtrl\t" 10 } } */

void
f1 (unsigned char *f, int o, unsigned char v)
{
  *f = (*f & ~(1 << o)) | (v << o);
}

void
f2 (unsigned char *f, int o, unsigned char v)
{
  int t = *f & ~(1 << o);
  *f = t | (v << o);
}

void
f3 (unsigned char *f, int o, unsigned char v)
{
  *f &= ~(1 << o);
}

void
f4 (unsigned char *f, int o, unsigned char v)
{
  *f = (*f & ~(1 << (o & 31))) | v;
}

void
f5 (unsigned char *f, int o, unsigned char v)
{
  *f = (*f & ~(1 << (o & 31))) | (v << (o & 31));
}

void
f6 (unsigned short *f, int o, unsigned short v)
{
  *f = (*f & ~(1 << o)) | (v << o);
}

void
f7 (unsigned short *f, int o, unsigned short v)
{
  int t = *f & ~(1 << o);
  *f = t | (v << o);
}

void
f8 (unsigned short *f, int o, unsigned short v)
{
  *f &= ~(1 << o);
}

void
f9 (unsigned short *f, int o, unsigned short v)
{
  *f = (*f & ~(1 << (o & 31))) | v;
}

void
f10 (unsigned short *f, int o, unsigned short v)
{
  *f = (*f & ~(1 << (o & 31))) | (v << (o & 31));
}
