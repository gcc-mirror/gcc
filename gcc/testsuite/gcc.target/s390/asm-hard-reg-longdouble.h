__attribute__ ((noipa))
long double
test_longdouble (long double x)
{
  long double y;
  asm ("sqxbr\t%0,%1" : "={f4}" (y) : "{f5}" (x));
  return y;
}

int
main (void)
{
  long double x = test_longdouble (42.L);
  long double y = 6.48074069840786023096596743608799656681773277430814773408787249757445105002862106857719481922686100006103515625L;
  if (x != y)
    __builtin_abort ();
  return 0;
}
