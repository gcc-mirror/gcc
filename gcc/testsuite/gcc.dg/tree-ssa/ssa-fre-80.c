/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

unsigned long a, b;
void foo (int m, int f)
{
  unsigned long tem = (unsigned long)m;
  a = tem + 1;
  if (f)
    {
      int tem2 = m + 1;
      b = (unsigned long)tem2;  /* Eliminated to a.  */
    }
}

/* { dg-final { scan-tree-dump-times "\\(long unsigned int\\)" 1 "fre1" } } */
