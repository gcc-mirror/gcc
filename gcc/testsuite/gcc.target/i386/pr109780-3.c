/* { dg-do run  { target avx2_runtime } }  */
/* { dg-options "-O2 -mavx2 -mtune=znver1 -fno-stack-protector -fno-stack-clash-protection" } */

char a;
static int b, c, f;
char *d = &a;
static char *e = &a;

__attribute__ ((weak))
void
g (int h, int i)
{
  int j = 1;
  for (; c != -3; c = c - 1)
    {
      int k[10];
      f = 0;
      for (; f < 10; f++)
        k[f] = 0;
      *d = k[1];
      if (i < *d)
        {
          *e = h;
          for (; j < 9; j++)
            {
              b = 1;
              for (; b < 7; b++)
                ;
            }
        }
    }
}

__attribute__ ((weak))
void
run (void)
{
  g (1, 1);
}

int
main (void)
{
  run ();
  return 0;
}
