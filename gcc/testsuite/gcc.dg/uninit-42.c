/* PR middle-end/101734 - missing warning reading from a write-only object
   Verify that reading objects pointed to by arguments
   declared with attribute access none or write-only is diagnosed by
   -Wmaybe-uninitialized.
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(mode, ...) __attribute__ ((access (mode, __VA_ARGS__)))

void sink (void *, ...);


A (write_only, 1, 2)
int nowarn_wo_assign_r0 (int *p, int n)
{
  *p = n;
  return *p;
}

A (write_only, 1, 2)
int nowarn_wo_sink_r0 (int *p, int n)
{
  sink (p, p + 1, p + n);
  return *p;
}

A (write_only, 1, 2)
int warn_wo_r0 (int *p, int n)
{
  return *p;        // { dg-warning "'\\*p' may be used uninitialized \\\[-Wmaybe-uninitialized" }
}


A (write_only, 1, 2)
int nowarn_wo_w1_r1 (int *p, int n)
{
  p[1] = n;
  return p[1];
}

A (write_only, 1, 2)
int warn_wo_r1 (int *p, int n)
{
  return p[1];      // { dg-warning "'p\\\[1]' may be used uninitialized" }
}


A (write_only, 1, 2)
int nowarn_wo_rwi_rj (int *p, int n, int i, int j)
{
  p[i] = n;
  return p[j];
}

A (write_only, 1, 2)
  int warn_wo_ri (int *p, int n, int i)
{
  return p[i];      // { dg-warning " may be used uninitialized" }
}



A (none, 1, 2)
int* nowarn_none_sink_return (int *p, int n)
{
  sink (p, p + 1, p + n);
  return p;
}

A (none, 1, 2)
int warn_none_r0 (int *p, int n)
{
  (void)&n;
  return *p;        // { dg-warning "'\\*p' may be used uninitialized" }
}

A (none, 1, 2)
int warn_none_r1 (int *p, int n)
{
  return p[1];      // { dg-warning "'p\\\[1]' may be used uninitialized" }
}

A (write_only, 1, 2)
int warn_none_ri (int *p, int n, int i)
{
  return p[i];      // { dg-warning " may be used uninitialized" }
}
