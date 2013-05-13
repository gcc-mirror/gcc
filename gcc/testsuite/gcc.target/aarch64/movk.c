/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);

long long int
dummy_number_generator ()
{
  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0xefff, lsl 16" } } */
  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0xc4cc, lsl 32" } } */
  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0xfffe, lsl 48" } } */
  return -346565474575675;
}

int
main (void)
{

  long long int num = dummy_number_generator ();
  if (num > 0)
    abort ();

  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0x4667, lsl 16" } } */
  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0x7a3d, lsl 32" } } */
  if (num / 69313094915135 != -5)
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
