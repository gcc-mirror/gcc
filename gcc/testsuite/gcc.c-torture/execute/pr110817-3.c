typedef unsigned __attribute__((__vector_size__ (1*sizeof(unsigned)))) V;

V v;
unsigned char c;

int
main (void)
{
  V x = (v > 0) > (v != c);
  volatile signed int t = x[0];
  if (t)
    __builtin_abort ();
  return 0;
}
