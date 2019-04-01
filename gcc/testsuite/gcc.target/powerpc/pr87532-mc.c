/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#include <stdio.h>

static vector unsigned __int128
deoptimize_uint128 (vector unsigned __int128  a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

static vector unsigned long long int
deoptimize_ulong (vector unsigned long long int a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

static vector unsigned int
deoptimize_uint (vector unsigned int a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

static vector unsigned char
deoptimize_uchar (vector unsigned char a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

static vector unsigned short
deoptimize_ushort (vector unsigned short a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

__attribute ((noinline)) unsigned __int128
get_auto_n_uint128 (vector unsigned __int128 a, int n)
{
  return __builtin_vec_extract (a, n);
}

__attribute ((noinline)) unsigned long long int
get_auto_n_ulong (vector unsigned long long int a, int n)
{
  return __builtin_vec_extract (a, n);
}

__attribute ((noinline))
unsigned int get_auto_n_uint (vector unsigned int a, int n)
{
  return __builtin_vec_extract (a, n);
}

__attribute ((noinline))
unsigned char get_auto_n_uchar (vector unsigned char a, int n)
{
  return __builtin_vec_extract (a, n);
}

__attribute ((noinline))
unsigned short get_auto_n_ushort (vector unsigned short a, int n)
{
  return __builtin_vec_extract (a, n);
}


int check_uint128_element (int i, unsigned __int128 entry)
{
  printf ("checking uint128 entry at index %d\n", i);

  return (entry == ((((unsigned __int128) 0xffeeddccbbaa9988ULL) << 64)
		    | 0x0706050403020100ULL));
}

int check_ulong_element (int i, unsigned long long int entry)
{
  printf ("checking ulong entry 0x%llx at index %d\n", entry, i);

  switch (i % 2)
    {
      case 0: return (entry == 0x9999901010ULL);
      case 1: return (entry == 0x7777733333ULL);
      default:
	return 0;
    }
}

int check_uint_element (int i, unsigned int entry)
{
  printf ("checking uint entry 0x%x at index %d\n", entry, i);

  switch (i % 4)
    {
    case 0: return (entry == 0x99999);
    case 1: return (entry == 0x01010);
    case 2: return (entry == 0x77777);
    case 3: return (entry == 0x33333);
    default:
      return 0;
    }
}

int check_uchar_element (int i, unsigned char entry)
{
  printf ("checking uchar entry 0x%x at index %d\n", entry, i);
  switch (i % 16)
    {
    case 0: return (entry == 0x90);
    case 1: return (entry == 0x80);
    case 2: return (entry == 0x70);
    case 3: return (entry == 0x60);
    case 4: return (entry == 0x50);
    case 5: return (entry == 0x40);
    case 6: return (entry == 0x30);
    case 7: return (entry == 0x20);
    case 8: return (entry == 0x10);
    case 9: return (entry == 0xf0);
    case 10: return (entry == 0xe0);
    case 11: return (entry == 0xd0);
    case 12: return (entry == 0xc0);
    case 13: return (entry == 0xb0);
    case 14: return (entry == 0xa0);
    case 15: return (entry == 0xff);
    default:
      return 0;
    }
}

int check_ushort_element (int i, unsigned short entry)
{
  printf ("checking ushort entry 0x%x at index %d\n", entry, i);
  switch (i % 8)
    {
    case 0: return (entry == 0x9988);
    case 1: return (entry == 0x8877);
    case 2: return (entry == 0x7766);
    case 3: return (entry == 0x6655);
    case 4: return (entry == 0x5544);
    case 5: return (entry == 0x4433);
    case 6: return (entry == 0x3322);
    case 7: return (entry == 0x2211);
    default:
      return 0;
    }
}

void do_auto_uint128 ( vector unsigned __int128 a )
{
  int i;
  unsigned __int128 c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uint128 (a,i);
      if (!check_uint128_element (i, c)) abort ();
    }
 }

void do_auto_ulong ( vector unsigned long long int a )
{
  int i;
  unsigned long long int c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_ulong (a,i);
      if (!check_ulong_element (i, c)) abort ();
    }
 }

void do_auto_uint ( vector unsigned int a )
{
  int i;
  unsigned int c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uint (a,i);
      if (!check_uint_element (i, c)) abort ();
    }
 }

void do_auto_ushort ( vector unsigned short a )
{
  int i;
  unsigned short c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_ushort (a,i);
      if (!check_ushort_element (i, c)) abort ();
    }
}

void do_auto_uchar ( vector unsigned char a )
{
  int i;
  unsigned char c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uchar (a,i);
      if (!check_uchar_element (i, c)) abort ();
    }
}

int
main (void)
{
  size_t i;

  vector unsigned __int128 u = {
    ((((unsigned __int128) 0xffeeddccbbaa9988ULL) << 64)
     | 0x0706050403020100ULL) };
  vector unsigned __int128 du;

  vector unsigned long long int v = { 0x9999901010ULL, 0x7777733333ULL };
  vector unsigned long long int dv;

  vector unsigned int x = { 0x99999, 0x01010, 0x77777, 0x33333 };
  vector unsigned int dx;

  vector unsigned char y = { 0x90, 0x80, 0x70, 0x60, 0x50, 0x40, 0x30, 0x20,
			     0x10, 0xf0, 0xe0, 0xd0, 0xc0, 0xb0, 0xa0, 0xff };
  vector unsigned char dy;

  vector unsigned short z = { 0x9988, 0x8877, 0x7766, 0x6655,
			      0x5544, 0x4433, 0x3322, 0x2211 };
  vector unsigned short dz;

  do_auto_uint128 (u);
  do_auto_ulong (v);
  do_auto_uint (x);
  do_auto_uchar (y);
  do_auto_ushort (z);

  du = deoptimize_uint128 (u);
  dv = deoptimize_ulong (v);
  dx = deoptimize_uint (x);
  dy = deoptimize_uchar (y);
  dz = deoptimize_ushort (z);

  do_auto_uint128 (du);
  do_auto_ulong (dv);
  do_auto_uint (dx);
  do_auto_uchar (dy);
  do_auto_ushort (dz);
  return 0;
}
