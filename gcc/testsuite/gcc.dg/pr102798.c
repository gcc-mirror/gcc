/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-pta" } */

typedef __SIZE_TYPE__ size_t;

__attribute__((__noipa__))
void BUF_reverse (unsigned char *out, const unsigned char *in, size_t size)
{
  size_t i;
  if (in)
    {
      out += size - 1;
      for (i = 0; i < size; i++)
        *out++ = *in++;
    }
  else
    {
      unsigned char *q;
      char c;
      q = out + size - 1;
      for (i = 0; i < size ; i++)
            {
              *out++ = 1;
            }
    }
}

int
main (void)
{
  unsigned char buf[40];
  unsigned char buf1[40];
  for (unsigned i = 0; i < sizeof (buf); i++)
    buf[i] = i;
  BUF_reverse (buf, 0, sizeof (buf));
  for (unsigned i = 0; i < sizeof (buf); i++)
    if (buf[i] != 1)
      __builtin_abort ();

  return 0;
}
