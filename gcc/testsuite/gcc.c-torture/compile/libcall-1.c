/* Failed on ARM because rtx_varies_p didn't like the REG_EQUAL notes
   generated for libcalls.
   http://gcc.gnu.org/ml/gcc-patches/2004-02/msg01518.html */
static const char digs[] = "0123456789ABCDEF";
int __attribute__((pure)) bar();

int foo (int i)
{
  int len;
  if (i)
    return 0;
  len = bar();
  return digs[len];
}
