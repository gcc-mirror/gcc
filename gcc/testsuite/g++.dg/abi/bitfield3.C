// Test for oversized bitfield alignment in structs on IA-32
// { dg-do run { target i?86-*-* } }
// { dg-options "-O2" }

struct A
{
  char a;
  int b : 224;	// { dg-warning "exceeds its type" "" }
  char c;
} a, a4[4];

struct B
{
  char d;
  A e;
  char f;
} b;

struct C
{
  char g;
  long long h : 64;
  char i;
} c, c4[4];

struct D
{
  char j;
  C k;
  char l;
} d;

struct E
{
  char m;
  long long n : 160;	// { dg-warning "exceeds its type" "" }
  char o;
} e, e4[4];

struct F
{
  char p;
  E q;
  char r;
} f;

int main (void)
{
  if (&a.c - &a.a != 32)
    return 1;
  if (sizeof (a) != 36)
    return 2;
  if (sizeof (a4) != 4 * 36)
    return 3;
  if (sizeof (b) != 2 * 4 + 36)
    return 4;
  if (__alignof__ (b.e) != 4)
    return 5;
  if (&c.i - &c.g != 16)
    return 6;
  if (sizeof (c) != 24)
    return 7;
  if (sizeof (c4) != 4 * 24)
    return 8;
  if (sizeof (d) != 2 * 8 + 24)
    return 9;
  if (__alignof__ (d.k) != 8)
    return 10;
  if (&e.o - &e.m != 28)
    return 11;
  if (sizeof (e) != 32)
    return 12;
  if (sizeof (e4) != 4 * 32)
    return 13;
  if (sizeof (f) != 2 * 8 + 32)
    return 14;
  if (__alignof__ (f.q) != 8)
    return 15;
  return 0;
}
