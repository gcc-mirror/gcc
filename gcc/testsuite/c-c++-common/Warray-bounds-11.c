/* PR c++/120954 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds=2" } */

static const int a[32] = { 11, 12, 13, 14, 15 };
static const int b[32] = { 21, 22, 23, 24, 25 };
static const int c[32] = { 31, 32, 33, 34, 35 };
static const int d[32] = { 111, 112, 113, 114, 115 };
static const int e[32] = { 121, 122, 123, 124, 125 };
static const int f[32] = { 131, 132, 133, 134, 135 };

int
foo (int x, int y)
{
  int r = 0;
  if (x >= 0 && x < 32)
    r = (y >= 4 ? (y >= 0x65 ? a : b ) : c)[x];
  else if (x >= 0x100 && x < 0x120)
    r = (y >= 4 ? (y >= 0x65 ? d : e ) : f)[x - 0x100];
  return r;
}
