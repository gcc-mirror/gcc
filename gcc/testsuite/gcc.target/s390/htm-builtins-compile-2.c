/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

void must_not_compile1 (void)
{
  __builtin_tabort (0); /* { dg-error "Invalid transaction abort code:" } */
}

void must_not_compile2 (void)
{
  __builtin_tabort (255); /* { dg-error "Invalid transaction abort code:" } */
}
