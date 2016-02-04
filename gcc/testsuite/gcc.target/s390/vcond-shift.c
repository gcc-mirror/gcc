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

#define SZ 4
#define SZ2 8
#define SZ3 16

void foo(int *w)
{
  int i;
  /* Should expand to (w + (w < 0 ? 1 : 0)) >> 1
     which in turn should get simplified to (w + (w >> 31)) >> 1.  */
  for (i = 0; i < SZ; i++)
    w[i] = w[i] / 2;
}

void foo2(short *w)
{
  int i;
  for (i = 0; i < SZ2; i++)
    w[i] = w[i] / 2;
}


void foo3(signed char *w)
{
  int i;
  for (i = 0; i < SZ3; i++)
    w[i] = w[i] / 2;
}

int baz(int *x)
{
  int i;
  for (i = 0; i < SZ; i++)
    x[i] = x[i] < 0 ? -1 : 0;
}

int baf(short *x)
{
  int i;
  for (i = 0; i < SZ2; i++)
    x[i] = x[i] >= 0 ? 0 : 1;
}

int bal(signed char *x)
{
  int i;
  for (i = 0; i < SZ3; i++)
    x[i] = x[i] >= 0 ? 0 : -1;
}
