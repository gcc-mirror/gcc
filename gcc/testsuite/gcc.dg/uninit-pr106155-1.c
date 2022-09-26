/* { dg-do compile } */
/* { dg-options "-O -fno-ivopts -Wuninitialized" } */

int *e;
int f1 (void);
void f2 (int);
long f3 (void *, long, int *);
void f4 (void *);
int *fh;

void tst (void)
{
  int status;
  unsigned char badData[3][3] = { { 7 }, { 16 }, { 23 } };
  int badDataSize[3] = { 1, 1, 1 };
  int i;

  for (i = 0; i < 3; i++)
    {
      int emax;
      if (i == 2)
        emax = f1 ();
      status = f3 (&badData[i][0], badDataSize[i], fh);
      if (status)
        {
          f1 ();
          f1 ();
          f1 ();
        }
      f4 (fh);
      *e = 0;
      f1 ();
      /* When threading the following out of the loop uninit
	 analysis needs to pick up the loop exit condition
	 to match up with this guard.
	 ???  This doesn't work reliably when IVOPTs is run.  */
      if (i == 2)
        f2 (emax);  /* { dg-bogus "uninitialized" } */
    }
}
