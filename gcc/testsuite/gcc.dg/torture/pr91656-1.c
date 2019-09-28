/* { dg-do run } */
/* { dg-additional-options "-fgcse-after-reload" } */

int a, b, c, d, e;

static __attribute__ ((__noipa__))
int foo (int i)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  __builtin_memmove (&i, &e, 1);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  __builtin_memmove ((char *) &i + sizeof (i) - 1,
		     (char *) &e + sizeof (e) - 1, 1);
#elif __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
  __builtin_memmove ((char *) &i + sizeof (i) - 2,
		     (char *) &e + sizeof (e) - 2, 1);
#else
#error "endian unknown?"
#endif
  if (a > 0)
    i /= e;
  e /= 5;
  b = 0;
  return i + c + d + 5;
}

int
main (void)
{
  int x = foo (4);
  if (x != 5)
    __builtin_abort ();
  return 0;
}
