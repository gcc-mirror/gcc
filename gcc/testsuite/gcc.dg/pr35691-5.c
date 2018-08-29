/* PR middle-end/35691 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\]\[\n\r]" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests \[ghi]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[ghi]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[ghi]1_\[0-9]*\\(D\\) \\+\\\[0, 0\\\]\[\n\r]" 1 "reassoc1" } } */

int
f1 (int x1, unsigned int y1, int z1, _Bool d, _Bool e, _Bool f, long long g1, unsigned long long h1, long long i1)
{
  int a = x1 == 0;
  int b = y1 == 0;
  int c = z1 == 0;
  int j = g1 == 0;
  int k = h1 == 0;
  int l = i1 == 0;
  return a && d && j && b && e && l && f && c && k;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]2_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]2_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]2_\[0-9]*\\(D\\) \\+\\\[0, 0\\\]\[\n\r]" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests \[ghi]2_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[ghi]2_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[ghi]2_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */

int
f2 (int x2, int y2, unsigned int z2, _Bool d, _Bool e, _Bool f, long long g2, unsigned long long h2, long long i2)
{
  int a = x2 == 0;
  int b = y2 == 0;
  int c = z2 == 0;
  int j = g2 == -1LL;
  int k = h2 == -1ULL;
  int l = i2 == -1LL;
  return !a || d || !l || !b || !k || e || f || !c || !j;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xyz]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xyz]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests \[ghi]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[ghi]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[ghi]3_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */

int
f3 (unsigned int x3, int y3, int z3, _Bool d, _Bool e, _Bool f, signed char g3, unsigned char h3, signed char i3)
{
  int a = x3 == -1U;
  int b = y3 == -1;
  int c = z3 == -1;
  int j = g3 == -1;
  int k = h3 == (unsigned char) -1U;
  int l = i3 == -1;
  return a && d && j && b && k && e && f && c && l;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]4_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xyz]4_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xyz]4_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */

int
f4 (int x4, unsigned int y4, unsigned int z4, _Bool d, _Bool e, _Bool f)
{
  int a = x4 == -1U;
  int b = y4 == -1U;
  int c = z4 == -1;
  return !a || d || !b || e || f || !c;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]5_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]5_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]5_\[0-9]*\\(D\\) \\+\\\[0, 0\\\]\[\n\r]" 1 "reassoc1" } } */

int
f5 (int x5, int y5, int z5, _Bool d, _Bool e, _Bool f)
{
  int a = x5 == 0;
  int b = y5 != 0;
  int c = z5 != 0;
  return a && d && !b && e && f && !c;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xyz]6_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]6_\[0-9]*\\(D\\) \\+\\\[0, 0\\\] and \[xyz]6_\[0-9]*\\(D\\) \\+\\\[0, 0\\\]\[\n\r]" 1 "reassoc1" } } */

int
f6 (unsigned int x6, unsigned int y6, unsigned int z6, _Bool d, _Bool e, _Bool f)
{
  int a = x6 == 0;
  int b = y6 != 0;
  int c = z6 != 0;
  return !a || d || b || e || f || c;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xy]7_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xy]7_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */

int
f7 (int x7, int y7, int z7, _Bool d, _Bool e, _Bool f)
{
  int a = x7 == -1;
  int b = y7 != -1;
  int c = z7 == -1;
  return a && d && !b && e && f && !c;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests \[xy]8_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\] and \[xy]8_\[0-9]*\\(D\\) \\+\\\[\[1-9-]\[0-9]*, \[1-9-]\[0-9]*\\\]\[\n\r]" 1 "reassoc1" } } */

int
f8 (unsigned int x8, unsigned int y8, unsigned int z8, _Bool d, _Bool e, _Bool f)
{
  int a = x8 == -1;
  int b = y8 != -1;
  int c = z8 == -1;
  return !a || d || b || e || f || c;
}

/* { dg-final { scan-tree-dump-not "Optimizing range tests \[xyz]9_\[0-9]*\\(D\\)" "reassoc1" } } */

int
f9 (int x9, int y9, int z9, _Bool d, _Bool e, _Bool f)
{
  int a = x9 == -1;
  int b = y9 == -1;
  int c = z9 == -1;
  return a || d || b || e || f || c;
}

/* { dg-final { scan-tree-dump-not "Optimizing range tests \[xyz]0_\[0-9]*\\(D\\)" "reassoc1" } } */

int
f0 (int x0, int y0, int z0, _Bool d, _Bool e, _Bool f)
{
  int a = x0 != 0;
  int b = y0 != 0;
  int c = z0 != 0;
  return a && d && b && e && f && c;
}
