/* { dg-do run } */
/* { dg-options "-Os -Wno-overflow -fno-tree-switch-conversion" } */

#include "exit-abort.h"
volatile unsigned char y;

unsigned char __attribute__((noinline,noclone))
foo1 (char x)
{
  switch (x)
    {
    case (char)0x11: y = 7; break;
    case (char)0x12: y = 4; break;
    case (char)0x13: y = 8; break;
    case (char)0x14: y = 21; break;
    case (char)0x15: y = 65; break;
    case (char)0x16: y = 27; break;
    case (char)0x17: y = 72; break;
    case (char)0x18: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo2 (char x)
{
  switch (x)
    {
    case 0x01: y = 7; break;
    case 0x02: y = 4; break;
    case 0x03: y = 8; break;
    case 0x04: y = 21; break;
    case 0x05: y = 65; break;
    case 0x06: y = 27; break;
    case 0x07: y = 72; break;
    case 0x08: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo3 (char x)
{
  switch (x)
    {
    case 0x1000001L: y = 7; break;
    case 0x1000002L: y = 4; break;
    case 0x1000003L: y = 8; break;
    case 0x1000004L: y = 21; break;
    case 0x1000005L: y = 65; break;
    case 0x1000006L: y = 27; break;
    case 0x1000007L: y = 72; break;
    case 0x1000008L: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo4 (char x)
{
  switch (x)
    {
    case 0x100000001LL: y = 7; break;
    case 0x100000002LL: y = 4; break;
    case 0x100000003LL: y = 8; break;
    case 0x100000004LL: y = 21; break;
    case 0x100000005LL: y = 65; break;
    case 0x100000006LL: y = 27; break;
    case 0x100000007LL: y = 72; break;
    case 0x100000008LL: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo5 (int x)
{
  switch (x)
    {
    case (char)0x11: y = 7; break;
    case (char)0x12: y = 4; break;
    case (char)0x13: y = 8; break;
    case (char)0x14: y = 21; break;
    case (char)0x15: y = 65; break;
    case (char)0x16: y = 27; break;
    case (char)0x17: y = 72; break;
    case (char)0x18: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo6 (int x)
{
  switch (x)
    {
    case 0x101: y = 7; break;
    case 0x102: y = 4; break;
    case 0x103: y = 8; break;
    case 0x104: y = 21; break;
    case 0x105: y = 65; break;
    case 0x106: y = 27; break;
    case 0x107: y = 72; break;
    case 0x108: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo7 (int x)
{
  switch (x)
    {
    case 0x1000001L: y = 7; break;
    case 0x1000002L: y = 4; break;
    case 0x1000003L: y = 8; break;
    case 0x1000004L: y = 21; break;
    case 0x1000005L: y = 65; break;
    case 0x1000006L: y = 27; break;
    case 0x1000007L: y = 72; break;
    case 0x1000008L: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo8 (int x)
{
  switch (x)
    {
    case 0x100000001LL: y = 7; break;
    case 0x100000002LL: y = 4; break;
    case 0x100000003LL: y = 8; break;
    case 0x100000004LL: y = 21; break;
    case 0x100000005LL: y = 65; break;
    case 0x100000006LL: y = 27; break;
    case 0x100000007LL: y = 72; break;
    case 0x100000008LL: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo9 (long x)
{
  switch (x)
    {
    case (char)0x11: y = 7; break;
    case (char)0x12: y = 4; break;
    case (char)0x13: y = 8; break;
    case (char)0x14: y = 21; break;
    case (char)0x15: y = 65; break;
    case (char)0x16: y = 27; break;
    case (char)0x17: y = 72; break;
    case (char)0x18: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo10 (unsigned long x)
{
  switch (x)
    {
    case 0x100: y = 39; break;
    case 0x101: y = 7; break;
    case 0x102: y = 4; break;
    case 0x103: y = 8; break;
    case 0x104: y = 21; break;
    case 0x105: y = 65; break;
    case 0x106: y = 27; break;
    case 0x107: y = 72; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo11 (long x)
{
  switch (x)
    {
    case 0x1000001L: y = 7; break;
    case 0x1000002L: y = 4; break;
    case 0x1000003L: y = 8; break;
    case 0x1000004L: y = 21; break;
    case 0x1000005L: y = 65; break;
    case 0x1000006L: y = 27; break;
    case 0x1000007L: y = 72; break;
    case 0x1000008L: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo12 (long x)
{
  switch (x)
    {
    case 0x100000001LL: y = 7; break;
    case 0x100000002LL: y = 4; break;
    case 0x100000003LL: y = 8; break;
    case 0x100000004LL: y = 21; break;
    case 0x100000005LL: y = 65; break;
    case 0x100000006LL: y = 27; break;
    case 0x100000007LL: y = 72; break;
    case 0x100000008LL: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo13 (long long x)
{
  switch (x)
    {
    case (char)0x11: y = 7; break;
    case (char)0x12: y = 4; break;
    case (char)0x13: y = 8; break;
    case (char)0x14: y = 21; break;
    case (char)0x15: y = 65; break;
    case (char)0x16: y = 27; break;
    case (char)0x17: y = 72; break;
    case (char)0x18: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo14 (long long x)
{
  switch (x)
    {
    case 0x101: y = 7; break;
    case 0x102: y = 4; break;
    case 0x103: y = 8; break;
    case 0x104: y = 21; break;
    case 0x105: y = 65; break;
    case 0x106: y = 27; break;
    case 0x107: y = 72; break;
    case 0x108: y = 39; break;
    default: y=0;
    }
  return y;
}

unsigned char __attribute__((noinline,noclone))
foo15 (long long x)
{
  switch (x)
    {
    case 0x1000001L: y = 7; break;
    case 0x1000002L: y = 4; break;
    case 0x1000003L: y = 8; break;
    case 0x1000004L: y = 21; break;
    case 0x1000005L: y = 65; break;
    case 0x1000006L: y = 27; break;
    case 0x1000007L: y = 72; break;
    case 0x1000008L: y = 39; break;
    default: y=0;
    }
  return y;
}


unsigned char __attribute__((noinline,noclone))
foo16 (long long x)
{
  switch (x)
    {
    case 0x100000001LL: y = 7; break;
    case 0x100000002LL: y = 4; break;
    case 0x100000003LL: y = 8; break;
    case 0x100000004LL: y = 21; break;
    case 0x100000005LL: y = 65; break;
    case 0x100000006LL: y = 27; break;
    case 0x100000007LL: y = 72; break;
    case 0x100000008LL: y = 39; break;
    default: y=0;
    }
  return y;
}

int main ()
{
  if (foo1 (0x13) != 8)
    abort();

  if (foo2 (0x06) != 27)
    abort();

  if (foo3 (0x02) != 4)
    abort();

  if (foo4 (0x01) != 7)
    abort();

  if (foo5 (0x15) != 65)
    abort();

  if (foo6 (0x103) != 8)
    abort();

  if (foo7 (0x04) != 21)
    abort();

  if (foo8 (0x07) != 72)
    abort();

  if (foo9 (0x10000011L) != 0)
    abort();

  if (foo10 (0x1000105L) != 0)
    abort();

  if (foo11 (0x1000008L) != 39)
    abort();

  if (foo12 (0x1000004L) != 0)
    abort();

  if (foo13 (0x109LL) != 0)
    abort();

  if (foo14 (0x108LL) != 39)
    abort();

  if (foo15 (0x1000001LL) != 7)
    abort();

  if (foo16 (0x100000004LL) != 21)
    abort();

  return 0;
}
