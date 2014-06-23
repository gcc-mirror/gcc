/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -save-temps" } */

extern void abort ();

#define force_simd_di(v) asm volatile ("mov %d0, %1.d[0]" :"=w" (v) :"w" (v) :)
#define force_simd_si(v) asm volatile ("mov %s0, %1.s[0]" :"=w" (v) :"w" (v) :)

typedef unsigned long long int UInt64x1;
typedef long long int Int64x1;
typedef unsigned int UInt32x1;
typedef int Int32x1;

UInt64x1
test_lshift_left_sisd_di (UInt64x1 b, UInt64x1 c)
{
  UInt64x1 a;

  force_simd_di (b);
  force_simd_di (c);
  a = b << 8;
  a = a << c;
  force_simd_di (a);
  return a;
}
/* { dg-final { scan-assembler "shl\td\[0-9\]+,\ d\[0-9\]+,\ 8" } } */
/* { dg-final { scan-assembler "ushl\td\[0-9\]+,\ d\[0-9\]+,\ d\[0-9\]+" } } */

UInt32x1
test_lshift_left_sisd_si (UInt32x1 b, UInt32x1 c)
{
  UInt32x1 a;

  force_simd_si (b);
  force_simd_si (c);
  a = b << 4;
  a = a << c;
  force_simd_si (a);
  return a;
}
/* { dg-final { scan-assembler "shl\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ 4" } } */
/* "ushl\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" (counted later) */

UInt64x1
test_lshift_right_sisd_di (UInt64x1 b, UInt64x1 c)
{
  UInt64x1 a;

  force_simd_di (b);
  force_simd_di (c);
  a = b >> 8;
  a = a >> c;
  force_simd_di (a);
  return a;
}
/* { dg-final { scan-assembler "ushr\td\[0-9\]+,\ d\[0-9\]+,\ 8" } } */
/* "neg\td\[0-9\]+,\ d\[0-9\]+" (counted later) */
/* { dg-final { scan-assembler "ushl\td\[0-9\]+,\ d\[0-9\]+,\ d\[0-9\]+" } } */

UInt64x1
test_lshift_right_sisd_si (UInt32x1 b, UInt32x1 c)
{
  UInt32x1 a;

  force_simd_si (b);
  force_simd_si (c);
  a = b >> 4;
  a = a >> c;
  force_simd_si (a);
  return a;
}
/* { dg-final { scan-assembler "ushr\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ 4" } } */
/* "neg\td\[0-9\]+,\ d\[0-9\]+" (counted later) */
/* { dg-final { scan-assembler-times "ushl\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" 2 } } */

Int64x1
test_ashift_right_sisd_di (Int64x1 b, Int64x1 c)
{
  Int64x1 a;

  force_simd_di (b);
  force_simd_di (c);
  a = b >> 8;
  a = a >> c;
  force_simd_di (a);
  return a;
}
/* { dg-final { scan-assembler "sshr\td\[0-9\]+,\ d\[0-9\]+,\ 8" } } */
/* "neg\td\[0-9\]+,\ d\[0-9\]+" (counted later) */
/* { dg-final { scan-assembler "sshl\td\[0-9\]+,\ d\[0-9\]+,\ d\[0-9\]+" } } */

Int32x1
test_ashift_right_sisd_si (Int32x1 b, Int32x1 c)
{
  Int32x1 a;

  force_simd_si (b);
  force_simd_si (c);
  a = b >> 4;
  a = a >> c;
  force_simd_si (a);
  return a;
}
/* { dg-final { scan-assembler "sshr\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ 4" } } */
/* { dg-final { scan-assembler-times "neg\td\[0-9\]+,\ d\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler "sshl\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" } } */


/* The following are to make sure if the integer instructions lsl/lsr/asr are
   generated in non-vector scenarios */

UInt64x1
test_lshift_left_int_di (UInt64x1 b, UInt64x1 c)
{
  UInt64x1 a;

  a = b << 8;
  a = a << c;
  return a;
}
/* { dg-final { scan-assembler "lsl\tx\[0-9\]+,\ x\[0-9\]+,\ 8" } } */
/* { dg-final { scan-assembler "lsl\tx\[0-9\]+,\ x\[0-9\]+,\ x\[0-9\]+" } } */

UInt32x1
test_lshift_left_int_si (UInt32x1 b, UInt32x1 c)
{
  UInt32x1 a;

  a = b << 4;
  a = a << c;
  return a;
}
/* { dg-final { scan-assembler "lsl\tw\[0-9\]+,\ w\[0-9\]+,\ 4" } } */
/* { dg-final { scan-assembler "lsl\tw\[0-9\]+,\ w\[0-9\]+,\ w\[0-9\]+" } } */

UInt64x1
test_lshift_right_int_di (UInt64x1 b, UInt64x1 c)
{
  UInt64x1 a;

  a = b >> 8;
  a = a >> c;
  return a;
}
/* { dg-final { scan-assembler "lsr\tx\[0-9\]+,\ x\[0-9\]+,\ 8" } } */
/* { dg-final { scan-assembler "lsr\tx\[0-9\]+,\ x\[0-9\]+,\ x\[0-9\]+" } } */

UInt32x1
test_lshift_right_int_si (UInt32x1 b, UInt32x1 c)
{
  UInt32x1 a;

  a = b >> 4;
  a = a >> c;
  return a;
}
/* { dg-final { scan-assembler "lsr\tw\[0-9\]+,\ w\[0-9\]+,\ 4" } } */
/* { dg-final { scan-assembler "lsr\tw\[0-9\]+,\ w\[0-9\]+,\ w\[0-9\]+" } } */

Int64x1
test_ashift_right_int_di (Int64x1 b, Int64x1 c)
{
  Int64x1 a;

  a = b >> 8;
  a = a >> c;
  return a;
}
/* { dg-final { scan-assembler "asr\tx\[0-9\]+,\ x\[0-9\]+,\ 8" } } */
/* { dg-final { scan-assembler "asr\tx\[0-9\]+,\ x\[0-9\]+,\ x\[0-9\]+" } } */

Int32x1
test_ashift_right_int_si (Int32x1 b, Int32x1 c)
{
  Int32x1 a;

  a = b >> 4;
  a = a >> c;
  return a;
}
/* { dg-final { scan-assembler "asr\tw\[0-9\]+,\ w\[0-9\]+,\ 4" } } */
/* { dg-final { scan-assembler "asr\tw\[0-9\]+,\ w\[0-9\]+,\ w\[0-9\]+" } } */

Int64x1
test_corners_sisd_di (Int64x1 b)
{
  force_simd_di (b);
  b = b >> 63;
  b = b >> 0;
  b += b >> 65; /* { dg-warning "right shift count >= width of type" } */
  force_simd_di (b);

  return b;
}
/* { dg-final { scan-assembler "sshr\td\[0-9\]+,\ d\[0-9\]+,\ 63" } } */

Int32x1
test_corners_sisd_si (Int32x1 b)
{
  force_simd_si (b);
  b = b >> 31;
  b = b >> 0;
  b += b >> 33; /* { dg-warning "right shift count >= width of type" } */
  force_simd_si (b);

  return b;
}
/* { dg-final { scan-assembler "sshr\tv\[0-9\]+\.2s,\ v\[0-9\]+\.2s,\ 31" } } */



#define CHECK(var,val) \
do                     \
  {                    \
    if (var != val)    \
      abort();         \
  }                    \
while(0)

UInt64x1 x = 0xC01dDeadBeefFaceull;
UInt32x1 y = 0xDeadBeef;

int
main ()
{
  x = test_lshift_left_sisd_di (x, 8);
  CHECK (x, 0xdeadbeefface0000ull);
  x = test_lshift_right_int_di (x, 8);
  CHECK (x, 0x0000deadbeeffaceull);
  x = test_lshift_right_sisd_di (x, 8);
  CHECK (x, 0x00000000deadbeefull);
  x = test_lshift_left_int_di (x, 8);
  CHECK (x, 0x0000deadbeef0000ull);
  x = ~x;
  x = test_ashift_right_int_di (x, 8);
  CHECK (x, 0xffffffff21524110ull);
  x = test_ashift_right_sisd_di (x, 8);
  CHECK (x, 0xffffffffffff2152ull);
  x = test_corners_sisd_di (x);
  CHECK (x, 0xfffffffffffffffeull);

  y = test_lshift_left_sisd_si (y, 4);
  CHECK (y, 0xadbeef00);
  y = test_lshift_right_int_si (y, 4);
  CHECK (y, 0x00adbeef);
  y = test_lshift_right_sisd_si (y, 4);
  CHECK (y, 0x0000adbe);
  y = test_lshift_left_int_si (y, 4);
  CHECK (y, 0x00adbe00);
  y = ~y;
  y = test_ashift_right_int_si (y, 4);
  CHECK (y, 0xffff5241);
  y = test_ashift_right_sisd_si (y, 4);
  CHECK (y, 0xffffff52);
  y = test_corners_sisd_si (y);
  CHECK (y, 0xfffffffe);

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
