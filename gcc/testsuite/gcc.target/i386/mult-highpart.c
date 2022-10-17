/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -Wno-long-long -fdump-tree-optimized" } */

typedef unsigned int __attribute ((mode(TI))) uti_t;
typedef int __attribute ((mode(TI))) ti_t;

long long stest1(long long x, long long y)
{
  return ((ti_t)x * (ti_t)y) >> 64;
}

long long stest2(long long x)
{
  return ((ti_t)x * 19065) >> 64;
}

long long stest3(long long x, long long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >> 64;
}

long long stest4(long long x)
{
  return (uti_t)((ti_t)x * 19065) >> 64;
}

ti_t stest5(long long x, long long y)
{
  return ((ti_t)x * (ti_t)y) >> 64;
}

ti_t stest6(long long x)
{
  return ((ti_t)x * 19065) >> 64;
}

uti_t stest7(long long x, long long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >>64;
}

uti_t stest8(long long x)
{
  return (uti_t)((ti_t)x * 19065) >> 64;
}

long long stest9(long long x, long long y)
{
  return ((ti_t)x * (ti_t)y) >> 72;
}

long long stest10(long long x)
{
  return ((ti_t)x * 19065) >> 72;
}

long long stest11(long long x, long long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >> 72;
}

long long stest12(long long x)
{
  return (uti_t)((ti_t)x * 19065) >> 72;
}

ti_t stest13(long long x, long long y)
{
  return ((ti_t)x * (ti_t)y) >> 72;
}

ti_t stest14(long long x)
{
  return ((ti_t)x * 19065) >> 72;
}

uti_t stest15(long long x, long long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >> 72;
}

uti_t stest16(long long x)
{
  return (uti_t)((ti_t)x * 19065) >> 72;
}

unsigned long long utest1(unsigned long long x, unsigned long long y)
{
  return ((uti_t)x * (uti_t)y) >> 64;
}

unsigned long long utest2(unsigned long long x)
{
  return ((uti_t)x * 19065) >> 64;
}

unsigned long long utest3(unsigned long long x, unsigned long long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >> 64;
}

unsigned long long utest4(unsigned long long x)
{
  return (ti_t)((uti_t)x * 19065) >> 64;
}

uti_t utest5(unsigned long long x, unsigned long long y)
{
  return ((uti_t)x * (uti_t)y) >> 64;
}

uti_t utest6(unsigned long long x)
{
  return ((uti_t)x * 19065) >> 64;
}

ti_t utest7(unsigned long long x, unsigned long long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >>64;
}

ti_t utest8(long long x)
{
  return (uti_t)((ti_t)x * 19065) >> 64;
}

unsigned long long utest9(unsigned long long x, unsigned long long y)
{
  return ((uti_t)x * (uti_t)y) >> 72;
}

unsigned long long utest10(unsigned long long x)
{
  return ((uti_t)x * 19065) >> 72;
}

unsigned long long utest11(unsigned long long x, unsigned long long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >> 72;
}

unsigned long long utest12(unsigned long long x)
{
  return (ti_t)((uti_t)x * 19065) >> 72;
}

uti_t utest13(unsigned long long x, unsigned long long y)
{
  return ((uti_t)x * (uti_t)y) >> 72;
}

uti_t utest14(unsigned long long x)
{
  return ((uti_t)x * 19065) >> 72;
}

ti_t utest15(unsigned long long x, unsigned long long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >> 72;
}

ti_t utest16(unsigned long long x)
{
  return (ti_t)((uti_t)x * 19065) >> 72;
}

/* { dg-final { scan-tree-dump-times " h\\* " 32 "optimized" } } */
