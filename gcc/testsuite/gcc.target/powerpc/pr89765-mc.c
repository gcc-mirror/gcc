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

__attribute ((noinline))
vector unsigned __int128
set_auto_n_uint128 (vector unsigned __int128 a, int n, unsigned __int128 x)
{
  return vec_insert (x, a, n);
}

__attribute ((noinline))
vector unsigned long long int
set_auto_n_ulong (vector unsigned long long int a, int n,
		  unsigned long long int x)
{
  return vec_insert (x, a, n);
}

__attribute ((noinline))
vector unsigned int
set_auto_n_uint (vector unsigned int a, int n, unsigned int x)
{
  return vec_insert (x, a, n);
}

__attribute ((noinline))
vector unsigned char
set_auto_n_uchar (vector unsigned char a, int n, unsigned char x)
{
  return vec_insert (x, a, n);
}

__attribute ((noinline))
vector unsigned short
set_auto_n_ushort (vector unsigned short a, int n, unsigned short x)
{
  return vec_insert (x, a, n);
}

__attribute ((noinline))
unsigned __int128
get_auto_n_uint128 (vector unsigned __int128 a, int n)
{
  return vec_extract (a, n);
}

__attribute ((noinline))
unsigned long long int
get_auto_n_ulong (vector unsigned long long int a, int n)
{
  return vec_extract (a, n);
}

__attribute ((noinline))
unsigned int
get_auto_n_uint (vector unsigned int a, int n)
{
  return vec_extract (a, n);
}

__attribute ((noinline))
unsigned char
get_auto_n_uchar (vector unsigned char a, int n)
{
  return vec_extract (a, n);
}

__attribute ((noinline))
unsigned short
get_auto_n_ushort (vector unsigned short a, int n)
{
  return vec_extract (a, n);
}

int check_uint128_element (int i, unsigned __int128 entry)
{
  printf ("checking uint128 entry at index %d\n", i);

  return (entry == ((((unsigned __int128) 0xffeeddccbbaa9988ULL) << 64)
		    | 0x0706050403020100ULL));
}

unsigned __int128 get_uint128_element (int i)
{
  return ((((unsigned __int128) 0xffeeddccbbaa9988ULL) << 64)
	  | 0x0706050403020100ULL);
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

unsigned long long int get_ulong_element (int i)
{
  switch (i % 2)
    {
    case 0: return 0x9999901010ULL;
    case 1: return 0x7777733333ULL;
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

unsigned int get_uint_element (int i)
{
  switch (i % 4)
    {
    case 0: return 0x99999;
    case 1: return 0x01010;
    case 2: return 0x77777;
    case 3: return 0x33333;
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

unsigned char get_uchar_element (int i)
{
  switch (i % 16)
    {
    case 0: return 0x90;
    case 1: return 0x80;
    case 2: return 0x70;
    case 3: return 0x60;
    case 4: return 0x50;
    case 5: return 0x40;
    case 6: return 0x30;
    case 7: return 0x20;
    case 8: return 0x10;
    case 9: return 0xf0;
    case 10: return 0xe0;
    case 11: return 0xd0;
    case 12: return 0xc0;
    case 13: return 0xb0;
    case 14: return 0xa0;
    case 15: return 0xff;
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

unsigned short get_ushort_element (int i)
{
  switch (i % 8)
    {
    case 0: return 0x9988;
    case 1: return 0x8877;
    case 2: return 0x7766;
    case 3: return 0x6655;
    case 4: return 0x5544;
    case 5: return 0x4433;
    case 6: return 0x3322;
    case 7: return 0x2211;
    }
}

vector unsigned __int128
init_auto_uint128 (vector unsigned __int128 a)
{
  int i;
  for (i = 0; i < 32; i += 3)
    a = set_auto_n_uint128 (a, i, get_uint128_element (i));
  return a;
}

void do_auto_uint128 (vector unsigned __int128 a)
{
  int i;
  unsigned __int128 c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uint128 (a, i);
      if (!check_uint128_element (i, c)) abort ();
    }
}

vector unsigned long long int
init_auto_ulong (vector unsigned long long int a)
{
  int i;
  for (i = 0; i < 32; i += 3)
    a = set_auto_n_ulong (a, i, get_ulong_element (i));
  return a;
}

void do_auto_ulong (vector unsigned long long int a)
{
  int i;
  unsigned long long int c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_ulong (a, i);
      if (!check_ulong_element (i, c)) abort ();
    }
 }

vector unsigned int init_auto_uint (vector unsigned int a)
{
  int i;
  for (i = 0; i < 32; i += 3)
    a = set_auto_n_uint (a, i, get_uint_element (i));
  return a;
}

void do_auto_uint (vector unsigned int a)
{
  int i;
  unsigned int c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uint (a, i);
      if (!check_uint_element (i, c)) abort ();
    }
 }

vector unsigned short init_auto_ushort ( vector unsigned short a )
{
  int i;
  for (i = 0; i < 32; i += 3)
    a = set_auto_n_ushort (a, i, get_ushort_element (i));
  return a;
}

void do_auto_ushort (vector unsigned short a)
{
  int i;
  unsigned short c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_ushort (a, i);
      if (!check_ushort_element (i, c)) abort ();
    }
}

vector unsigned char init_auto_uchar (vector unsigned char a)
{
  int i;
  for (i = 0; i < 32; i += 3)
    a = set_auto_n_uchar (a, i, get_uchar_element (i));
  return a;
}

void do_auto_uchar (vector unsigned char a)
{
  int i;
  unsigned char c;
  for (i = 0; i < 32; i += 3)
    {
      c = get_auto_n_uchar (a, i);
      if (!check_uchar_element (i, c)) abort ();
    }
}

int
main (void)
{
  size_t i;

  vector unsigned __int128 u = { 0 };
  vector unsigned __int128 du;

  vector unsigned long long int v = { 0, 0 };
  vector unsigned long long int dv;

  vector unsigned int x = { 0, 0, 0, 0 };
  vector unsigned int dx;

  vector unsigned char y = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  vector unsigned char dy;

  vector unsigned short z = { 0, 0, 0, 0, 0, 0, 0, 0 };
  vector unsigned short dz;

  du = init_auto_uint128 (u);
  dv = init_auto_ulong (v);
  dx = init_auto_uint (x);
  dy = init_auto_uchar (y);
  dz = init_auto_ushort (z);

  du = deoptimize_uint128 (du);
  dv = deoptimize_ulong (dv);
  dx = deoptimize_uint (dx);
  dy = deoptimize_uchar (dy);
  dz = deoptimize_ushort (dz);

  do_auto_uint128 (du);
  do_auto_ulong (dv);
  do_auto_uint (dx);
  do_auto_uchar (dy);
  do_auto_ushort (dz);
  return 0;
}
