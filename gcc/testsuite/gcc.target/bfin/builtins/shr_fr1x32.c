/* { dg-do run } */

typedef int fract32;

extern void abort (void);

int main ()
{
  fract32 f;

  f = __builtin_bfin_shr_fr1x32 (0x87654321, 4);
  if (f != 0xf8765432)
    abort ();

  return 0;
}
