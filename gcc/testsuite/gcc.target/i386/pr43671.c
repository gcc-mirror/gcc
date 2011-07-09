/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mtune=i686 -O1 -fpeel-loops -fschedule-insns2 -ftree-vectorize -fsched2-use-superblocks" } */

extern void abort ();

int main ()
{
  struct {
    char ca[16];
  } s;
  int i;

  for (i = 0; i < 16; i++)
    {
      s.ca[i] = 5;
    }


  for (i = 0; i < 16; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}
