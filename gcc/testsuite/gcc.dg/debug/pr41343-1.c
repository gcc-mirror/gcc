/* { dg-do compile } */

#define X(new,old) int i ## new = i ## old + i ## old;
#define Y(pfx) X(pfx ## 1, pfx) \
  X(pfx ## 2, pfx ## 1) \
  X(pfx ## 3, pfx ## 2) \
  X(pfx ## 4, pfx ## 3) \
  X(pfx ## 5, pfx ## 4) \
  X(pfx ## 6, pfx ## 5) \
  X(pfx ## 7, pfx ## 6) \
  X(pfx ## 8, pfx ## 7) \
  X(pfx ## 9, pfx ## 8)

void foo (int i1)
{
  Y(1)
  Y(11)
  Y(111)
  asm ("" : : "X" (i1));
}
