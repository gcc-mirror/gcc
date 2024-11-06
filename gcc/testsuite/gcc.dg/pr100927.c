/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -ftrapping-math -fdump-tree-optimized -fdump-rtl-final" } */
/* { dg-final { scan-tree-dump-times {(?n)= \(int\)} 3 "optimized" } }  */
/* { dg-final { scan-rtl-dump-times {(?n)^(?!.*REG_EQUIV)(?=.*\(fix:SI)} 3 "final" } }  */

int
foo_ofr ()
{
  union {float a;
    int b;}c;
  c.b = 0x4f000000;
  return (int)c.a;
}

int
foo_inf ()
{
  union {float a;
    int b;}c;
  c.b = 0xff800000;
  return (int)c.a;
}

int
foo_nan ()
{
  union {float a;
    int b;}c;
  c.b = 0xff800001;
  return (int)c.a;
}
