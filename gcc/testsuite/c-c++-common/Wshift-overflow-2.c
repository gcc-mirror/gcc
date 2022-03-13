/* PR c++/55095 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O -Wno-shift-overflow" } */

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)
#define INTM2 (sizeof (int) * __CHAR_BIT__ - 2)
#define LLONGM1 (sizeof (long long) * __CHAR_BIT__ - 1)
#define LLONGM2 (sizeof (long long) * __CHAR_BIT__ - 2)

#define INT_MIN (-__INT_MAX__-1)
#define LONG_LONG_MIN (-__LONG_LONG_MAX__-1)

int i1 = 1 << INTM1;
int i2 = 9 << INTM1;
int i3 = 10 << INTM2;
int i4 = __INT_MAX__ << 2;
int i5 = __INT_MAX__ << INTM1;
int i6 = -1 << INTM1;
int i7 = -9 << INTM1;
int i8 = -10 << INTM2;
int i9 = -__INT_MAX__ << 2;
int i10 = -__INT_MAX__ << INTM1;
int i11 = INT_MIN << 1;

int r1 = 1 >> INTM1;
int r2 = 9 >> INTM1;
int r3 = 10 >> INTM2;
int r4 = __INT_MAX__ >> 2;
int r5 = __INT_MAX__ >> INTM1;
int r6 = -1 >> INTM1;
int r7 = -9 >> INTM1;
int r8 = -10 >> INTM2;
int r9 = -__INT_MAX__ >> 2;
int r10 = -__INT_MAX__ >> INTM1;

unsigned u1 = 1 << INTM1;
unsigned u2 = 9 << INTM1;
unsigned u3 = 2U << INTM1;
unsigned u4 = 9U << INTM1;
unsigned u5 = 10U << INTM2;

long long int l1 = 1LL << LLONGM1;
long long int l2 = 9LL << LLONGM1;
long long int l3 = 10LL << LLONGM2;
long long int l4 = __LONG_LONG_MAX__ << 2;
long long int l5 = __LONG_LONG_MAX__ << LLONGM1;
long long int l6 = -1LL << LLONGM1;
long long int l7 = -9LL << LLONGM1;
long long int l8 = -10LL << LLONGM2;
long long int l9 = -__LONG_LONG_MAX__ << 2;
long long int l10 = -__LONG_LONG_MAX__ << LLONGM1;
long long int l11 = LONG_LONG_MIN << 1;

void
fn (void)
{
  const int a = 10;
  const __SIZE_TYPE__ b = INTM1;
  int k1 = a << b;
  int k2 = 10 << b;
  int k3 = a << INTM1;
}
