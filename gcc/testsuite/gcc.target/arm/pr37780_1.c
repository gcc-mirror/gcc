/* Test that we can remove the conditional move due to CLZ
   being defined at zero.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6t2_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v6t2 } */

int
fooctz (int i)
{
  return (i == 0) ? 32 : __builtin_ctz (i);
}

int
fooctz2 (int i)
{
  return (i != 0) ? __builtin_ctz (i) : 32;
}

unsigned int
fooctz3 (unsigned int i)
{
  return (i > 0) ?  __builtin_ctz (i) : 32;
}

/* { dg-final { scan-assembler-times "rbit\t*" 3 } } */

int
fooclz (int i)
{
  return (i == 0) ? 32 : __builtin_clz (i);
}

int
fooclz2 (int i)
{
  return (i != 0) ? __builtin_clz (i) : 32;
}

unsigned int
fooclz3 (unsigned int i)
{
  return (i > 0) ? __builtin_clz (i) : 32;
}

/* { dg-final { scan-assembler-times "clz\t" 6 } } */
/* { dg-final { scan-assembler-not "cmp\t.*0" } } */
