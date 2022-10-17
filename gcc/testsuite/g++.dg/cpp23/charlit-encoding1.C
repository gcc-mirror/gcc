// PR c++/102615 - P2316R2 - Consistent character literal encoding
// { dg-do run }

extern "C" void abort ();

int
main ()
{
#if ' ' == 0x20
  if (' ' != 0x20)
    abort ();
#elif ' ' == 0x40
  if (' ' != 0x40)
    abort ();
#else
  if (' ' == 0x20 || ' ' == 0x40)
    abort ();
#endif
#if 'a' == 0x61
  if ('a' != 0x61)
    abort ();
#elif 'a' == 0x81
  if ('a' != 0x81)
    abort ();
#elif 'a' == -0x7F
  if ('a' != -0x7F)
    abort ();
#else
  if ('a' == 0x61 || 'a' == 0x81 || 'a' == -0x7F)
    abort ();
#endif
  return 0;
}
