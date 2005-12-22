// Test for bitfield alignment in structs on IA-32
// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }
// { dg-options "-O2" }
// { dg-options "-mno-align-double -mno-ms-bitfields" { target i?86-*-interix* i?86-*-cygwin* i?86-*-mingw*} }

extern void abort (void);
extern void exit (int);

struct A
{
  char a;
  long long b : 61;
  char c;
} a, a4[4];

struct B
{
  char d;
  struct A e;
  char f;
} b;

struct C
{
  char g;
  union U
  {
    char u1;
    long long u2;
    long long u3 : 64;
  } h;
  char i;
} c;

int main (void)
{
  if (&a.c - &a.a != 12)
    abort ();
  if (sizeof (a) != 16)
    abort ();
  if (sizeof (a4) != 4 * 16)
    abort ();
  if (sizeof (b) != 2 * 4 + 16)
    abort ();
  if (__alignof__ (b.e) != 4)
    abort ();
  if (&c.i - &c.g != 12)
    abort ();
  if (sizeof (c) != 16)
    abort ();
  if (__alignof__ (c.h) != 4)
    abort ();
  exit (0);
}
