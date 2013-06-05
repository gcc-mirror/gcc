/* { dg-do compile } */

int *a, *b;

void foo()
{
  // This seems like it should be ok.
  // Must check with standards people.
#pragma simd
  for (auto int autoi = 0; autoi < 1000; ++autoi)
    b[autoi] = a[autoi] * 2;
  // Similarly here.
  auto int autoj;
#pragma simd
  for (auto int autoj = 0; autoj < 1000; ++autoj)
    b[autoj] = a[autoj] * 2;
}
