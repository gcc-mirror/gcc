/* { dg-do compile }  */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3" } */

int a, d = 55003;
long c = 0, h;
long e = 1;
short i;

int
main ()
{
  for (int g = 0; g < 16; g++)
    {
      d |= c;
      short l = d;
      i = l < 0 || a >> 4 ? d : a;
      h = i - 8L;
      e &= h;
    }

  if (e != 1)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-not "vmv\.v\.i\tv\[0-9\],0" } } */
