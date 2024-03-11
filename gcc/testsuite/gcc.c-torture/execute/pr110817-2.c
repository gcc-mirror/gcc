
typedef unsigned char u8;
typedef unsigned __attribute__((__vector_size__ (8))) V;

V v;
unsigned char c;

int
main (void)
{
  V x = (v > 0) > (v != c);
 // V x = foo ();
  if (x[0] || x[1])
    __builtin_abort ();
  return 0;
}
