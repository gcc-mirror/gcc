/* { dg-do run } */
/* { dg-options "-O2" } */

void *x (void *pdst, const void *psrc, unsigned int pn)
{
  register void *return_dst = pdst;
  register unsigned char *dst = pdst;
  register unsigned const char *src = psrc;
  register int n __asm__ ("ebx") = pn;

  if (src < dst && dst < src + n)
    {
      src += n;
      dst += n;
      while (n--)
        *--dst = *--src;
      return return_dst;
    }

  while (n >= 16) n--;

  return return_dst;
}
extern void abort ();
extern void exit (int);
char xx[30] = "abc";
int main (void)
{
  char yy[30] = "aab";

  if (x (xx + 1, xx, 2) != xx + 1 || memcmp (xx, yy, sizeof (yy)) != 0)
    abort ();
  exit (0);
}
