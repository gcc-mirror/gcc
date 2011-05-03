/* { dg-do run } */

typedef int fract32;

extern void abort (void);

int main ()
{
  fract32 f;

  f = __builtin_bfin_shl_fr1x32 (0x12345678, 4);
  if (f != 0x7fffffff)
    abort ();

  return 0;
}
