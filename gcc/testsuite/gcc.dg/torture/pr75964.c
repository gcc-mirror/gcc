/* { dg-do run } */

typedef __UINT8_TYPE__ uint8_t;

uint8_t __attribute__ ((noinline, noclone))
abs8 (uint8_t x)
{
  if (x & 0x80)
    x = -x;

  if (x & 0x80)
    x = 0x7f;

  return x;
}

int
main (void)
{
  if (abs8 (0) != 0
      || abs8 (1) != 1
      || abs8 (127) != 127
      || abs8 (128) != 127
      || abs8 (129) != 127
      || abs8 (255) != 1)
    __builtin_abort ();
  return 0;
}
