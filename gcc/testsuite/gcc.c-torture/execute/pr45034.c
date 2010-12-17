extern void abort (void);
static void fixnum_neg(signed char x, signed char *py, int *pv)
{
  unsigned char ux, uy;

  ux = (unsigned char)x;
  uy = -ux;
  *py = (uy <= 127) ? (signed char)uy : (-(signed char)(255 - uy) - 1);
  *pv = (x == -128) ? 1 : 0;
}

void __attribute__((noinline)) foo(int x, int y, int v)
{
  if (y < -128 || y > 127)
    abort();
}

int test_neg(void)
{
  signed char x, y;
  int v, err;

  err = 0;
  x = -128;
  for (;;) {
      fixnum_neg(x, &y, &v);
      foo((int)x, (int)y, v);
      if ((v && x != -128) || (!v && x == -128))
	++err;
      if (x == 127)
	break;
      ++x;
  }
  return err;
}

int main(void)
{
  if (sizeof (char) != 1)
    return 0;
  if (test_neg() != 0)
    abort();
  return 0;
}

