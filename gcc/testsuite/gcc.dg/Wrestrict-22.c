/* { dg-do compile } */
/* { dg-options "-O2 -Wrestrict" } */

void test_memcpy_warn (char *d, unsigned n)
{
  for (unsigned i = n; i < 30; ++i)
    if (i > 10)
      __builtin_memcpy (d, d + 2, i); /* { dg-warning "overlaps" } */
}
