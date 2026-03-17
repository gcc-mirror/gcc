/* { dg-require-effective-target arm_arch_v7a_thumb_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_arch_v7a_thumb } */

extern int bar ();
extern void bar2 (int);

int
foo4 ()
{
  int result = 0;
  int f = -1;
  f = bar ();
  if (f < 0)
    {
      result = 1;
      goto bail;
    }
  bar ();
 bail:
  bar2 (f);
  return result;
}

/* { dg-final { scan-assembler-times "sub" 1 } } */
/* { dg-final { scan-assembler-times "cmp" 0 } } */
