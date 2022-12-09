#include <string.h>
#include "analyzer-decls.h"

/* Function for thwarting expansion of memmove by optimizer.  */

typedef void * (*memmove_t) (void *dst, const void *src, size_t n);
  
static memmove_t __attribute__((noinline))
get_memmove (void)
{
  return memmove;
}

void *test_1 (void *dst, void *src, size_t n)
{
  void *result = memmove (dst, src, n);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

void *test_1a (void *dst, void *src, size_t n)
{
  void *result = __memmove_chk (dst, src, n, -1);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

void *test_1b (void *dst, void *src, size_t n)
{
  memmove_t fn = get_memmove ();
  void *result = fn (dst, src, n);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

void test_2 (int i)
{
  int j;
  memmove (&j, &i, sizeof (int));
  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
}

void test_2a (int i)
{
  int j;
  __memmove_chk (&j, &i, sizeof (int), sizeof (int));
  __analyzer_eval (i == j);  /* { dg-warning "TRUE" } */
}

void test_2b (int i)
{
  int j;
  memmove_t fn = get_memmove ();
  fn (&j, &i, sizeof (int));
  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
}

void test_3 (void *src, size_t n)
{
  char buf[40], other[40];
  buf[0] = 'a';
  other[0] = 'b';
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "TRUE" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */

  memmove (buf, src, n);
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */
}

void test_3b (void *src, size_t n)
{
  char buf[40], other[40];
  memmove_t fn = get_memmove ();
  buf[0] = 'a';
  other[0] = 'b';
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "TRUE" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */

  fn (buf, src, n);
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */
}

/* Overwriting a zeroed buffer, then memmove of the result.  */

void test_4 (int a, int b)
{
  int src[1024];
  int dst[1024];
  memset (src, 0, sizeof (src));
  src[42] = a;
  src[100] = b;
  __analyzer_eval (src[0] == 0);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[42] == a);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[100] == b);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[1023] == 0);    /* { dg-warning "TRUE" } */

  memmove (dst, src, sizeof (src));
  __analyzer_eval (dst[0] == 0);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[42] == a);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[100] == b);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[1023] == 0);    /* { dg-warning "TRUE" } */  
}

void test_4b (int a, int b)
{
  int src[1024];
  int dst[1024];
  memmove_t fn = get_memmove ();
  memset (src, 0, sizeof (src));
  src[42] = a;
  src[100] = b;
  __analyzer_eval (src[0] == 0);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[42] == a);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[100] == b);    /* { dg-warning "TRUE" } */
  __analyzer_eval (src[1023] == 0);    /* { dg-warning "TRUE" } */

  fn (dst, src, sizeof (src));
  __analyzer_eval (dst[0] == 0);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[42] == a);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[100] == b);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dst[1023] == 0);    /* { dg-warning "TRUE" } */  
}

/* Populating a buffer from an unknown buffer.  */

void test_5 (void *src, size_t sz)
{
  char dst[1024];
  memmove (dst, src, sizeof (dst));
  __analyzer_eval (dst[0] == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (dst[1023] == 0); /* { dg-warning "UNKNOWN" } */
}

void test_5b (void *src, size_t sz)
{
  char dst[1024];
  memmove_t fn = get_memmove ();
  fn (dst, src, sizeof (dst));
  __analyzer_eval (dst[0] == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (dst[1023] == 0); /* { dg-warning "UNKNOWN" } */
}

/* Zero-sized memmove.  */

void test_6 (void *dst, void *src)
{
  memmove (dst, src, 0);
}

void test_6b (void *dst, void *src)
{
  memmove_t fn = get_memmove ();
  fn (dst, src, 0);
}

/* memmove to string literal.  */

void test_7 (void *src, size_t sz)
{
  memmove ((void *)"hello world", src, sz); /* { dg-warning "write to string literal" } */
}

void test_7b (void *src, size_t sz)
{
  memmove ((void *)"hello world", src, sz); /* { dg-warning "write to string literal" } */
}

/* memcpy from uninitialized buffer.  */

void test_8a (void *dst)
{
  char src[16];
  memmove (dst, src, 16); /* { dg-warning "use of uninitialized value" } */
}

void test_8b (void *dst, size_t n)
{
  char src[16];
  memmove (dst, src, n); /* { dg-warning "use of uninitialized value" } */
}
