/* { dg-do compile } */

int j_;
int jpgDecode_convert (unsigned i)
{
  __label__ label;
  int j;

  inline void __attribute__((always_inline,leaf)) f(void)
    {
      g();
    }

  void __attribute__((noinline)) read_buf_open (void)
    {
      goto label;
    }

  if (i != 0)
    f ();
  j = j_;
  read_buf_open ();
label:
  return j;
}
