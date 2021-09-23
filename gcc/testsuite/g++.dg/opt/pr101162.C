// PR tree-optimization/101162
// { dg-do compile }
// { dg-options "-O2" }

struct A { int i1, i2, i3, i4, i5, i6; };

int A::*
foo (int i)
{
  switch (i)
    {
    case 1: return &A::i1;
    case 2: return &A::i2;
    case 3: return &A::i3;
    case 4: return &A::i4;
    case 5: return &A::i5;
    case 6: return &A::i6;
    }

  return 0;
}
