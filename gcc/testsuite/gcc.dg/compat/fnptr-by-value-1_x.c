#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

typedef void (*fpi)(int);
typedef void (*fpd)(double);

extern void test1a (fpi);
extern void test1b (fpi, int);
extern void test1c (double, fpd);
extern void test2a (fpi, fpd);
extern void test2b (fpi, fpd, int);
extern void test2c (fpi, int, fpd);
extern void test2d (int, fpi, fpd);
extern void test2e (fpi, fpd, int, double);
extern void test2f (fpi, int, fpd, double);
extern void test2g (fpi, int, double, fpd);
extern void test2h (double, fpd, fpi, int);
extern void test2i (double, fpd, int, fpi);
extern void test2j (int, double, fpi, fpd);
extern void testva (int, ...);

int f1_val;
double f2_val;

void f1 (int i) { f1_val = i; }
void f2 (double x) { f2_val = x; }

void
checki (int x, int v)
{
  if (x != v)
    DEBUG_CHECK
}

void
checkd (double x, double v)
{
  if (x != v)
    DEBUG_CHECK
}

void
testit (void)
{
  DEBUG_FPUTS ("test1a: ");
  test1a (f1);
  checki (f1_val, 1);
  DEBUG_NL;
  DEBUG_FPUTS ("test1b: ");
  test1b (f1, 2);
  checki (f1_val, 2);
  DEBUG_NL;
  DEBUG_FPUTS ("test1c: ");
  test1c (3.0, f2);
  checkd (f2_val, 3.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2a: ");
  test2a (f1, f2);
  checki (f1_val, 10);
  checkd (f2_val, 10.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2b: ");
  test2b (f1, f2, 11);
  checki (f1_val, 11);
  checkd (f2_val, 11.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2c: ");
  test2c (f1, 12, f2);
  checki (f1_val, 12);
  checkd (f2_val, 12.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2d: ");
  test2d (13, f1, f2);
  checki (f1_val, 13);
  checkd (f2_val, 13.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2e: ");
  test2e (f1, f2, 14, 15.0);
  checki (f1_val, 14);
  checkd (f2_val, 15.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2f: ");
  test2f (f1, 16, f2, 17.0);
  checki (f1_val, 16);
  checkd (f2_val, 17.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2g: ");
  test2g (f1, 18, 19.0, f2);
  checki (f1_val, 18);
  checkd (f2_val, 19.0);
  DEBUG_NL;
  DEBUG_FPUTS ("test2h: ");
  test2h (20.0, f2, f1, 21);
  checkd (f2_val, 20.0);
  checki (f1_val, 21);
  DEBUG_NL;
  DEBUG_FPUTS ("test2i: ");
  test2i (22.0, f2, 23, f1);
  checkd (f2_val, 22.0);
  checki (f1_val, 23);
  DEBUG_NL;
  DEBUG_FPUTS ("test2j: ");
  test2j (24, 25.0, f1, f2);
  checki (f1_val, 24);
  checkd (f2_val, 25.0);
  if (test_va)
    {
      DEBUG_NL;
      DEBUG_FPUTS ("testva: ");
      testva (1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (2, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (3, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (4, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (5, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (6, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (7, f1, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (8, f1, f1, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (9, f1, f1, f1, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (10, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (11, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1);
      DEBUG_NL;
      DEBUG_FPUTS ("        ");
      testva (12, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1, f1);
    }
  DEBUG_NL;
}

void
fnptr_by_value_1_x ()
{
  DEBUG_INIT
  testit ();
  DEBUG_FINI

  if (fails != 0)
    abort ();
}
