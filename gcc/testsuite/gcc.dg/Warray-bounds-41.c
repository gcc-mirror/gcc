/* PR tree-optimization/89662- -Warray-bounds ICE on void* arithmetic
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void* vptr (void *c)
{
  return c;
}

void sink (void*);

void test_vptr_arith_vla_cst (void)
{
  int n = 1;
  char c[n];
  sink (vptr (c) - 1);    /* { dg-warning "\\\[-Warray-bounds" } */
}

void test_vptr_arith_vla_range (int n)
{
  if (n < 1 || 4 < n)
    return;

  char c[n];
  sink (vptr (c) - 1);    /* { dg-warning "\\\[-Warray-bounds" "pr82608" { xfail *-*-* } } */
}

void test_vptr_arith_vla_var (int n)
{
  char c[n];
  sink (vptr (c) - 1);    /* { dg-warning "\\\[-Warray-bounds" "pr82608" { xfail *-*-* } } */
}

