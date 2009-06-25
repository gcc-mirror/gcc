// { dg-do compile }
// { dg-options "-Wsign-compare" }

enum E { A, B, C };
extern void f1(int);
void
f2(E v1, E v2)
{
  for (unsigned int i = v1; i <= v2; ++i)
    f1(i);
  for (int i = v1; i <= v2; ++i)
    f1(i);
}
