/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

unsigned char a, b, d, f, g;

int test (void);

int
baz (int c)
{
  if (c == 0) return test ();
  if (b & 1)
    {
      g = 0;
      int e = (a & 0x0f) - (g & 0x0f);

      if (!a)  b |= 0x80;
      a = e + test ();
     f = g/5 + a*3879 + b *2985;
    }
   else
   {
     f = g + a*39879 + b *25;
   }
  return test ();
}
