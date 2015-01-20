/* { dg-do compile } */
/* { dg-options "-O -mgpopt=local" } */

extern int a __attribute__ ((section (".sdata")));
static volatile int b __attribute__ ((section (".sdata"))) = 1;
extern int c __attribute__ ((section (".data")));
static volatile int d __attribute__ ((section (".data"))) = 2;

extern int e;
static volatile int f = 3;

volatile int g __attribute__ ((weak)) = 4;

extern int h[100];
static int i[100];
static int j[100] __attribute__ ((section (".sdata")));

typedef int (*ftype) (int);
extern int foo (int);

extern int bar (int, int*, int*, int*, ftype);

int baz (void)
{
  return bar (a + b + c + d + e + f + g, h, i, j, foo);
}

/* { dg-final { scan-assembler "%gprel\\(a\\)" } } */
/* { dg-final { scan-assembler "%gprel\\(b\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(c\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(d\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(e\\)" } } */
/* { dg-final { scan-assembler "%gprel\\(f\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(g\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(h\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(i\\)" } } */
/* { dg-final { scan-assembler "%gprel\\(j\\)" } } */
/* { dg-final { scan-assembler-not "%gprel\\(foo\\)" } } */
