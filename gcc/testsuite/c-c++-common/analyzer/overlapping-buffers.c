/* Test of -Wanalyzer-overlapping-buffers.  */

#include <string.h>

/* Use noinline functions to hide these calls from the optimizer, to avoid
   undefined behavior being optimized away to GIMPLE_NOP before the analyzer
   sees it.  */

char *  __attribute__((noinline))
call_strcat_symbolic_1 (char *dest, const char *src)
{
  return strcat (dest, src); /* { dg-warning "overlapping buffers" } */
}

void test_strcat_symbolic_1 (char *p)
{
  call_strcat_symbolic_1 (p, p);
}

char *  __attribute__((noinline))
call_strcpy_symbolic_1 (char *dest, const char *src)
{
  return strcpy (dest, src); /* { dg-warning "overlapping buffers" } */
}

void test_strcpy_symbolic_1 (char *p)
{
  call_strcpy_symbolic_1 (p, p);
}

void *  __attribute__((noinline))
call_memcpy_concrete_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-warning "overlapping buffers" } */
}

void test_memcpy_concrete_1 (void *p)
{
  call_memcpy_concrete_1 (p, p, 10);
}

void *  __attribute__((noinline))
call_memcpy_symbolic_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-warning "overlapping buffers" } */
}

void test_memcpy_symbolic_1 (void *p, size_t n)
{
  call_memcpy_symbolic_1 (p, p, n);
}

/* Intersecting vs non-intersecting parts of the same buffer.  */

void *  __attribute__((noinline))
call_memcpy_nonintersecting_concrete_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-bogus "overlapping buffers passed as" } */
}

void test_memcpy_nonintersecting_concrete_1 (char *p)
{
  call_memcpy_nonintersecting_concrete_1 (p, p + 10, 10);
}

void *  __attribute__((noinline))
call_memcpy_nonintersecting_concrete_2 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-bogus "overlapping buffers passed as" } */
}

void test_memcpy_nonintersecting_concrete_2 (char *p)
{
  call_memcpy_nonintersecting_concrete_2 (p + 10, p, 10);
}

void *  __attribute__((noinline))
call_memcpy_intersecting_concrete_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-warning "overlapping buffers" } */
}

void test_memcpy_intersecting_concrete_1 (char *p)
{
  call_memcpy_intersecting_concrete_1 (p, p + 9, 10);
}

void *  __attribute__((noinline))
call_memcpy_intersecting_concrete_2 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-warning "overlapping buffers" } */
}

void test_memcpy_intersecting_concrete_2 (char *p)
{
  call_memcpy_intersecting_concrete_2 (p + 9, p, 10);
}

void *  __attribute__((noinline))
call_memcpy_intersecting_symbolic_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-warning "overlapping buffers" "" { xfail *-*-* } } */
  // TODO(xfail)
}

void test_memcpy_intersecting_symbolic_1 (char *p, size_t n)
{
  call_memcpy_intersecting_symbolic_1 (p, p + 1, n);
}

void *  __attribute__((noinline))
call_memcpy_nonintersecting_symbolic_1 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-bogus "overlapping buffers passed as" } */
}

void test_memcpy_nonintersecting_symbolic_1 (char *p, size_t n)
{
  call_memcpy_nonintersecting_symbolic_1 (p, p + n, n);
}

void *  __attribute__((noinline))
call_memcpy_nonintersecting_symbolic_2 (void *dest, const void *src, size_t n)
{
  return memcpy (dest, src, n); /* { dg-bogus "overlapping buffers passed as" } */
}

void test_memcpy_nonintersecting_symbolic_2 (char *p, size_t n)
{
  call_memcpy_nonintersecting_symbolic_2 (p + n, p, n);
}
/* It's OK for memmove's arguments to overlap.  */

void *  __attribute__((noinline))
call_memmove_symbolic_1 (void *dest, const void *src, size_t n)
{
  return memmove (dest, src, n); /* { dg-bogus "overlapping buffers passed as" } */
}

void test_memmove_symbolic_1 (void *p, size_t n)
{
  call_memmove_symbolic_1 (p, p, n);
}

static char *  __attribute__((noinline))
call_strncpy_1 (char *dest, const char *src, size_t n)
{
  return strncpy (dest, src, n); /* { dg-warning "overlapping buffers" } */
}

void
test_strncpy_1 (char *p, size_t n)
{
  call_strncpy_1 (p, p, n);
}
