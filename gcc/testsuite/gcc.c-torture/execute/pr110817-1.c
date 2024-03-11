typedef unsigned long __attribute__((__vector_size__ (8))) V;


V c;

int
main (void)
{
  V v = ~((V) { } <=0);
  if (v[0])
    __builtin_abort ();
  return 0;
}
