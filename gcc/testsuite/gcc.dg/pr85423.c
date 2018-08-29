/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2 -fvar-tracking-assignments -fno-guess-branch-probability -fno-peephole2 -fno-ssa-phiopt -fno-tree-pre --param max-jump-thread-duplication-stmts=8 -w" } */
/* { dg-additional-options "-march=nano" { target i?86-*-* x86_64-*-* } } */

int vn, xm;

void
i1 (int);

void
mb (int *ap, int ev)
{
  while (vn < 1)
    {
      i1 (vn);

      ev += *ap && ++vn;

      while (xm < 1)
        ++xm;

      if (*ap == 0)
        *ap = ev;

      ++vn;
    }
}
