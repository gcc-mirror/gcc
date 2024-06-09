/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=address" } */

int __seg_gs m;

int foo (void)
{
  return m;
}

extern int  __seg_gs n;

int bar (void)
{
  return n;
}

int baz (int __seg_gs *o)
{
  return *o;
}

/* { dg-final { scan-assembler-not "asan_report_load" } } */
