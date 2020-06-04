/* PR middle-end/95136 - missing -Wuninitialized on an array access with
   a variable offset
   { dg-do compile }
   { dg-options "-O -Wall" } */

#define NOIPA __attribute__ ((noipa))

NOIPA int a1_addr_varidx_plus_cst (int i)
{
  int a[4];              // { dg-message "'a' declared here" }
  int *p = &a[i + 1];
  return *p;             // { dg-warning "'a|a\\\[<unknown>]' is used uninitialized" }
}

NOIPA int a1_plus_addr_varidx_cst (int i)
{
  int a[4];              // { dg-message "'a' declared here" }
  int *p = &a[i] + 1;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}

NOIPA int a1_plus_addr_cstidx_var (int i)
{
  int a[4];              // { dg-message "'a' declared here" }
  int *p = &a[1] + i;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}

NOIPA int a1_plus_addr_varidx_var (int i, int j)
{
  int a[4];              // { dg-message "'a' declared here" }
  int *p = &a[i] + j;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}


NOIPA int a2_addr_varidx_plus_cst (int i, int j)
{
  int a[4][4];           // { dg-message "'a' declared here" }
  int *p = &a[i + 1][j + 1];
  return *p;             // { dg-warning "'a|a\\\[<unknown>]\\\[<unknown>]' is used uninitialized" }
}

NOIPA int a2_plus_addr_varidx_cst (int i, int j)
{
  int a[4][4];           // { dg-message "'a' declared here" }
  int *p = &a[i][j] + 1;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}

NOIPA int a2_plus_addr_cstidx_var (int i)
{
  int a[4][4];           // { dg-message "'a' declared here" }
  int *p = &a[1][1] + i;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}

NOIPA int a2_plus_addr_varidx_var (int i, int j, int k)
{
  int a[4][4];           // { dg-message "'a' declared here" }
  int *p = &a[i][j] + k;
  return *p;             // { dg-warning "'a' is used uninitialized" }
}
