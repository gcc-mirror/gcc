/* Check if conditional vector instructions are simplified
   into shift operations.  */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

/* { dg-final { scan-assembler "vesraf\t%v.?,%v.?,31" } } */
/* { dg-final { scan-assembler "vesrah\t%v.?,%v.?,15" } } */
/* { dg-final { scan-assembler "vesrab\t%v.?,%v.?,7" } } */
/* { dg-final { scan-assembler-not "vzero\t*" } } */
/* { dg-final { scan-assembler "vesrlf\t%v.?,%v.?,31" } } */
/* { dg-final { scan-assembler "vesrlh\t%v.?,%v.?,15" } } */
/* { dg-final { scan-assembler "vesrlb\t%v.?,%v.?,7" } } */

#define SZ 8
#define SZ2 16
#define SZ3 32

void foo(int *w)
{
  int i;
  int *ww = __builtin_assume_aligned (w, 8);

  /* Should expand to (ww + (ww < 0 ? 1 : 0)) >> 1
     which in turn should get simplified to (ww + (ww >> 31)) >> 1.  */
  for (i = 0; i < SZ; i++)
    ww[i] = ww[i] / 2;
}

void foo2(short *w)
{
  int i;
  short *ww = __builtin_assume_aligned (w, 8);

  for (i = 0; i < SZ2; i++)
    ww[i] = ww[i] / 2;
}


void foo3(signed char *w)
{
  int i;
  signed char *ww = __builtin_assume_aligned (w, 8);

  for (i = 0; i < SZ3; i++)
    ww[i] = ww[i] / 2;
}

int baz(int *x)
{
  int i;
  int *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < SZ; i++)
    xx[i] = xx[i] < 0 ? -1 : 0;
}

int baf(short *x)
{
  int i;
  short *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < SZ2; i++)
    xx[i] = xx[i] >= 0 ? 0 : 1;
}

int bal(signed char *x)
{
  int i;
  signed char *xx = __builtin_assume_aligned (x, 8);

  for (i = 0; i < SZ3; i++)
    xx[i] = xx[i] >= 0 ? 0 : -1;
}
