/* PR libstdc++/93121 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

union U { int a[3]; short c[6]; struct S { int d; int a : 2; int f : 1; int b : 24; int c : 5; int e; } b; };
const union U u = { .b = { 0x7efa3412, 3, 0, 0x50eca8, 0xb, 0x1eeffeed } };
const union U v = { .b = { 0x7efa3412, 1, 1, 0x7feedb, 0x5, 0x1eeffeed } };
union W { struct T { long long int a, b : 11, c : 3, d : 37, e : 1, f : 10, g : 2, h; } a; int b[6]; short c[12]; long long d[3]; };
const union W w = { .a = { 0x0feedbacdeadbeefLL, -1011, 2, -0xbacdeadbeLL, -1, 721, 1, 0x0feedbacdeadbeefLL } };
int a, b, c, d, e, f, g, h, i, j, k, l;
long long m;

void
foo ()
{
  a = u.a[1];
  b = v.a[1];
  c = u.c[2];
  d = u.c[3];
  e = v.c[2];
  f = v.c[3];
  g = w.b[2];
  h = w.b[3];
  i = w.c[4];
  j = w.c[5];
  k = w.c[6];
  l = w.c[7];
  m = w.d[1];
}

/* { dg-final { scan-tree-dump-times "a = 1518822723;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "b = 738162397;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "c = 25923;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "d = 23175;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "e = 30429;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "f = 11263;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "g = 1418761229;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "h = 1830622408;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "i = -27635;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "j = 21648;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "k = 5320;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "l = 27933;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "m = 7862463375103529997;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "a = -904030965;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "b = 1878907749;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "c = -13795;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "d = -27381;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "e = 28669;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "f = -9371;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "g = -2119529884;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "h = 709385029;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "i = -32342;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "j = -30108;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "k = 10824;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "l = 23365;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "m = -9103311533965288635;" 1 "optimized" { target be } } } */
