// PR c++/55095
// { dg-do compile { target c++14 } }
// { dg-options "-Wshift-overflow=2" }
// { dg-require-effective-target int32plus }

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)
#define LLONGM1 (sizeof (long long) * __CHAR_BIT__ - 1)

int i1 = 1 << INTM1;
unsigned u1 = 1 << INTM1;
long long int l1 = 1LL << LLONGM1;
