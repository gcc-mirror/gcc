/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fschedule-insns -fsel-sched-pipelining -fselective-scheduling -fno-if-conversion -fno-tree-dce" } */
/* { dg-additional-options "-march=bdver1" { target i?86-*-* x86_64-*-* } } */

int ov, rq, ac;

int
y2 (int);

void
f8 (int vn)
{
  while (rq < 1)
    {
      ov *= y2 (ac);
      vn += (!!ov && !!ac) + ac;
    }
}
