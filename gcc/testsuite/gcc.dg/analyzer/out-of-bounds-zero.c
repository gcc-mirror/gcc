/* { dg-additional-options "-Wno-stringop-overflow"} */
/* -Wstringop-overflow= triggers on test5.  */

#include <stdint.h>
#include <stdlib.h>

void test1 (void)
{
  int32_t buf[1];
  /* Zero bytes written on non-zero allocation.  */
  __builtin_memset (buf, 0, 0);
}

void test2 (void)
{
  /* ISO C forbids zero-size arrays but GCC compiles this to an
     zero-sized array without -Wpedantic.  */
  int32_t buf[0];
  /* Write on zero capacity.  */
  __builtin_memset (buf, 0, sizeof (int32_t)); /* { dg-line test2 } */

  /* { dg-warning "overflow" "warning" { target *-*-* } test2 } */
  /* { dg-message "from byte 0 till byte 3" "final event" { target *-*-* } test2 } */
}

void test3 (void)
{
  int32_t buf[0];
  /* Zero bytes written on zero capacity.  */
  __builtin_memset (buf, 0, 0);
}

void test4 (void)
{
  int32_t *buf = malloc (sizeof (int32_t));
  if (!buf)
    return;

  /* Zero bytes written on non-zero allocation.  */
  __builtin_memset (buf, 0, 0);
  free (buf);
}

void test5 (void)
{
  int32_t *buf = malloc (0);
  if (!buf)
    return;

  /* Write on zero capacity.  */
  __builtin_memset (buf, 0, sizeof (int32_t)); /* { dg-line test5 } */
  free (buf);

  /* { dg-warning "overflow" "warning" { target *-*-* } test5 } */
  /* { dg-message "from byte 0 till byte 3" "final event" { target *-*-* } test5 } */
}

void test6 (void)
{
  int32_t *buf = malloc (0);
  if (!buf)
    return;

  /* Zero bytes written on zero capacity.  */
  __builtin_memset (buf, 0, 0);
  free (buf);
}
