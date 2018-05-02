/* { dg-do compile } */
/* { dg-options "-O2 -Wrestrict" } */

unsigned n;

void test_memcpy_warn (char *d)
{
  if (n > 10 && n < 20)
    __builtin_memcpy (d, d + 2, n); /* { dg-warning "overlaps between" } */
}
