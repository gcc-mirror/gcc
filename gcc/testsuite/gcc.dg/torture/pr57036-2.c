/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */

int j_;
void g (void);
int jpgDecode_convert (unsigned i)
{
  __label__ label;
  int j;

  inline void __attribute__((always_inline,leaf)) f(void)
    {		/* { dg-warning "'leaf' attribute has no effect" } */
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
