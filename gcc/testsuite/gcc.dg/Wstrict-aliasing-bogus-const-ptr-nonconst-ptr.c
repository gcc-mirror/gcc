/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */

const int* foo (void)
{
  int *i;
  const int** cpi = (const int**) &i; /* { dg-bogus "const vs. non-const" } */
  i = 0;
  return *cpi;
}
