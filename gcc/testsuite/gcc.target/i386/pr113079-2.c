/* { dg-do run } */
/* { dg-options "-O2 -mavxvnniint8 -mavxvnni -mavxvnniint16" } */
/* { dg-require-effective-target avxvnniint16 } */
/* { dg-require-effective-target avxvnniint8 } */

#define AVXVNNIINT16
#define AVXVNNIINT8
#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK
#include "pr113079.c"

#define N 256

short hs1[4], hs2[4];
unsigned short hu1[4], hu2[4];
char qs1[16], qs2[16];
unsigned char qu1[16], qu2[16];

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
usdot_prodv4hi_scalar (unsigned short *a, short *b, int c)
{
  int i;
  for (i = 0; i < 4; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
udot_prodv4hi_scalar (unsigned short *a, unsigned short *b, int c)
{
  int i;
  for (i = 0; i < 4; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
sdot_prodv4hi_scalar (short *a, short *b, int c)
{
  int i;
  for (i = 0; i < 4; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
usdot_prodv8qi_scalar (unsigned char *a, char *b, int c)
{
  int i;
  for (i = 0; i < 8; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
udot_prodv8qi_scalar (unsigned char *a, unsigned char *b, int c)
{
  int i;
  for (i = 0; i < 8; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
sdot_prodv8qi_scalar (char *a, char *b, int c)
{
  int i;
  for (i = 0; i < 8; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

void init ()
{
  int i;

  for (i = 0; i < 4; i++)
    {
      hs1[i] = -i + 2;
      hs2[i] = -i * 2;
      hu1[i] = i * 3;
      hu2[i] = i * 4;
    }

  for (i = 0; i < 8; i++)
    {
      qs1[i] = -i + 2;
      qs2[i] = -i * 2;
      qu1[i] = i * 3;
      qu2[i] = i * 4;
    }

}

void
TEST (void)
{
  init ();
  int usdot_prodv8qi_ref;
  int sdot_prodv8qi_ref;
  int udot_prodv8qi_ref;
  int usdot_prodv4hi_ref;
  int sdot_prodv4hi_ref;
  int udot_prodv4hi_ref;

  int usdot_prodv8qi_exp;
  int sdot_prodv8qi_exp;
  int udot_prodv8qi_exp;
  int usdot_prodv4hi_exp;
  int sdot_prodv4hi_exp;
  int udot_prodv4hi_exp;

  usdot_prodv8qi_ref = usdot_prodv8qi (qu1, qs1, 1);
  usdot_prodv8qi_exp = usdot_prodv8qi_scalar (qu1, qs1, 1);
  if (usdot_prodv8qi_ref != usdot_prodv8qi_exp)
    abort ();

  udot_prodv8qi_ref = udot_prodv8qi (qu1, qu2, 2);
  udot_prodv8qi_exp = udot_prodv8qi_scalar (qu1, qu2, 2);
  if (udot_prodv8qi_ref != udot_prodv8qi_exp)
    abort ();

  sdot_prodv8qi_ref = sdot_prodv8qi (qs1, qs2, 3);
  sdot_prodv8qi_exp = sdot_prodv8qi_scalar (qs1, qs2, 3);
  if (sdot_prodv8qi_ref != sdot_prodv8qi_exp)
    abort ();

  usdot_prodv4hi_ref = usdot_prodv4hi (hu1, hs1, 4);
  usdot_prodv4hi_exp = usdot_prodv4hi_scalar (hu1, hs1, 4);
  if (usdot_prodv4hi_ref != usdot_prodv4hi_exp)
    abort ();

  udot_prodv4hi_ref = udot_prodv4hi (hu1, hu2, 5);
  udot_prodv4hi_exp = udot_prodv4hi_scalar (hu1, hu2, 5);
  if (udot_prodv4hi_ref != udot_prodv4hi_exp)
    abort ();

  sdot_prodv4hi_ref = sdot_prodv4hi (hs1, hs2, 6);
  sdot_prodv4hi_exp = sdot_prodv4hi_scalar (hs1, hs2, 6);
  if (sdot_prodv4hi_ref != sdot_prodv4hi_exp)
    abort ();
}
