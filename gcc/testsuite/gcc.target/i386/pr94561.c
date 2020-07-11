/* PR target/94561 */
/* { dg-do compile } */
/* { dg-options "-march=knl -O3 -funroll-loops" } */

struct xi {
  long int mg;
  int lx;
};

struct xi *di;
int *eu;

void
he (void);

int
m8 (int we, int i8)
{
  int wd, cj = 0;

  for (wd = 0; wd < 80; ++wd)
    {
      if (di->mg == 0 && (eu[wd] | !!we) == 0 && di->lx == 0)
        continue;

      if (i8 == 0)
        he ();

      ++cj;
    }

  return cj;
}

/* { dg-final { scan-assembler-not "vmov\[^\n\r]*%\[xy\]mm1\[6-9\].*" } } */
/* { dg-final { scan-assembler-not "vmov\[^\n\r]*%\[xy\]mm\[23\]\[0-9\].*" } } */
