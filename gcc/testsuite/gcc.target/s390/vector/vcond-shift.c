/* Check if conditional vector instructions are simplified
   into shift operations.  */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

/* { dg-final { scan-assembler-times "vesraf\t%v.?,%v.?,31" 6 } } */
/* { dg-final { scan-assembler-times "vesrah\t%v.?,%v.?,15" 6 } } */
/* { dg-final { scan-assembler-times "vesrab\t%v.?,%v.?,7" 6 } } */
/* { dg-final { scan-assembler-not "vzero\t*" } } */
/* { dg-final { scan-assembler-times "vesrlf\t%v.?,%v.?,31" 4 } } */
/* { dg-final { scan-assembler-times "vesrlh\t%v.?,%v.?,15" 4 } } */
/* { dg-final { scan-assembler-times "vesrlb\t%v.?,%v.?,7" 4 } } */

/* Make it expand to two vector operations.  */
#define ITER(X) (2 * (16 / sizeof (X[1])))

void
vesraf_div (int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  /* Should expand to (xx + (xx < 0 ? 1 : 0)) >> 1
     which in turn should get simplified to (xx + (xx >> 31)) >> 1.  */
  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] / 2;
}

void
vesrah_div (short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] / 2;
}


void
vesrab_div (signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] / 2;
}



int
vesraf_lt (int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? -1 : 0;
}

int
vesrah_lt (short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? -1 : 0;
}

int
vesrab_lt (signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? -1 : 0;
}



int
vesraf_ge (int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : -1;
}

int
vesrah_ge (short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : -1;
}

int
vesrab_ge (signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : -1;
}



int
vesrlf_lt (int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? 1 : 0;
}

int
vesrlh_lt (short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? 1 : 0;
}

int
vesrlb_lt (signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] < 0 ? 1 : 0;
}



int
vesrlf_ge (int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : 1;
}

int
vesrlh_ge (short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : 1;
}

int
vesrlb_ge (signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < ITER (xx); i++)
    xx[i] = xx[i] >= 0 ? 0 : 1;
}
