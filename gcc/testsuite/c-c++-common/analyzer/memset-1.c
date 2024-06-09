#include <string.h>
#include "analyzer-decls.h"

/* Zero-fill of uninitialized buffer.  */

void test_1 (void)
{
  char buf[256];
  void *p = memset (buf, 0, 256);
  __analyzer_eval (buf[42] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (p == buf); /* { dg-warning "TRUE" } */
}

/* As above, but with __builtin_memset.  */

void test_1a (void)
{
  char buf[256];
  __builtin_memset (buf, 0, 256);
  __analyzer_eval (buf[42] == 0); /* { dg-warning "TRUE" } */
}

/* Zero-fill of partially initialized buffer.  */

void test_2 (void)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  memset (buf, 0, 256);
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "TRUE" } */
}

/* A "memset" with known non-zero value.  */

void test_3 (int val)
{
  char buf[256];
  memset (buf, 'A', 256);
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
}

/* A "memset" with unknown value.  */

void test_4 (char val)
{
  char buf[256];
  memset (buf, val, 256);
  __analyzer_eval (buf[42] == (char)val); /* { dg-warning "TRUE" } */
}

/* A "memset" with unknown num bytes.  */

void test_5 (int n)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  memset (buf, 0, n);

  /* We can't know if buf[42] was overwritten by the memset or not.  */
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "UNKNOWN" } */
}

/* As test_5, but with "__builtin___memset_chk".  */

void test_5a (int n)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  __builtin___memset_chk (buf, 0, n, __builtin_object_size (buf, 0));

  /* We can't know if buf[42] was overwritten by the memset or not.  */
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "UNKNOWN" } */
}

/* A "memset" with unknown value, but with zero size.  */

static size_t __attribute__((noinline))
get_zero (void)
{
  return 0;
}

void test_6 (int val)
{
  char buf[256];
  buf[42] = 'A';
  memset (buf, 'B', get_zero ());
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */  
}

void test_6b (int val)
{
  char buf[256];
  memset (buf, 'A', sizeof (buf));
  memset (buf, 'B', get_zero ());
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */  
}

/* A "memset" of known size that's not the full buffer.  */

void test_7 (void)
{
  char buf[256];
  buf[128] = 'A';
  memset (buf, 0, 128);
  __analyzer_eval (buf[0] == '\0'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[127] == '\0'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[128] == 'A'); /* { dg-warning "TRUE" } */
}

void test_8 (void)
{
  char buf[20];
  memset (buf + 0, 0, 1);
  memset (buf + 1, 1, 1);
  memset (buf + 2, 2, 1);
  memset (buf + 3, 3, 1);
  memset (buf + 4, 4, 2);
  memset (buf + 6, 6, 2);
  memset (buf + 8, 8, 4);
  memset (buf + 12, 12, 8);
  __analyzer_eval (buf[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 6); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 6); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[8] == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[9] == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[10] == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[11] == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[12] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[13] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[14] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[15] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[16] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[17] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[18] == 12); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[19] == 12); /* { dg-warning "TRUE" } */
}

/* Various overlapping memset calls with different sizes and values.  */

void test_9 (void)
{
  char buf[8];
  memset (buf, 0, 8);
  __analyzer_eval (buf[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 0); /* { dg-warning "TRUE" } */

  memset (buf + 1, 1, 4);  
  __analyzer_eval (buf[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 0); /* { dg-warning "TRUE" } */

  memset (buf + 2, 2, 4);  
  __analyzer_eval (buf[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 0); /* { dg-warning "TRUE" } */

  memset (buf + 4, 3, 3);  
  __analyzer_eval (buf[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 0); /* { dg-warning "TRUE" } */

  memset (buf + 0, 4, 3);  
  __analyzer_eval (buf[0] == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 0); /* { dg-warning "TRUE" } */
}
