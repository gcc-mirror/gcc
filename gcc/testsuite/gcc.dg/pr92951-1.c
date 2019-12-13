/* PR rtl-optimization/92591 */
/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched -fweb -fno-dce -fno-ivopts -fno-sched-pressure -fno-tree-loop-distribute-patterns --param sms-dfa-history=1" } */
/* { dg-additional-options "-mcpu=e500mc" { target { powerpc-*-* } } } */

void
wf (char *mr, int tc)
{
  while (tc-- > 0)
    *mr++ = 0;
}
