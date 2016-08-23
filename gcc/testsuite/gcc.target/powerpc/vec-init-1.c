/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS -1, 2, 0, -123456
#define SPLAT 0x01234567

vector int sv = (vector int) { ELEMENTS };
vector int splat = (vector int) { SPLAT, SPLAT, SPLAT, SPLAT };
vector int sv_global, sp_global;
static vector int sv_static, sp_static;
static const int expected[] = { ELEMENTS };

extern void check (vector int a)
  __attribute__((__noinline__));

extern void check_splat (vector int a)
  __attribute__((__noinline__));

extern vector int pack_reg (int a, int b, int c, int d)
  __attribute__((__noinline__));

extern vector int pack_from_ptr (int *p_a, int *p_b, int *p_c, int *p_d)
  __attribute__((__noinline__));

extern vector int pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector int *p, int a, int b, int c, int d)
  __attribute__((__noinline__));

extern void pack_static (int a, int b, int c, int d)
  __attribute__((__noinline__));

extern void pack_global (int a, int b, int c, int d)
  __attribute__((__noinline__));

extern vector int splat_reg (int a)
  __attribute__((__noinline__));

extern vector int splat_from_ptr (int *p)
  __attribute__((__noinline__));

extern vector int splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector int *p, int a)
  __attribute__((__noinline__));

extern void splat_static (int a)
  __attribute__((__noinline__));

extern void splat_global (int a)
  __attribute__((__noinline__));

void
check (vector int a)
{
  size_t i;

  for (i = 0; i < 4; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector int a)
{
  size_t i;

  for (i = 0; i < 4; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector int
pack_reg (int a, int b, int c, int d)
{
  return (vector int) { a, b, c, d };
}

vector int
pack_from_ptr (int *p_a, int *p_b, int *p_c, int *p_d)
{
  return (vector int) { *p_a, *p_b, *p_c, *p_d };
}

vector int
pack_const (void)
{
  return (vector int) { ELEMENTS };
}

void
pack_ptr (vector int *p, int a, int b, int c, int d)
{
  *p = (vector int) { a, b, c, d };
}

void
pack_static (int a, int b, int c, int d)
{
  sv_static = (vector int) { a, b, c, d };
}

void
pack_global (int a, int b, int c, int d)
{
  sv_global = (vector int) { a, b, c, d };
}

vector int
splat_reg (int a)
{
  return (vector int) { a, a, a, a };
}

vector int
splat_from_ptr (int *p)
{
  return (vector int) { *p, *p, *p, *p };
}

vector int
splat_const (void)
{
  return (vector int) { SPLAT, SPLAT, SPLAT, SPLAT };
}

void
splat_ptr (vector int *p, int a)
{
  *p = (vector int) { a, a, a, a };
}

void
splat_static (int a)
{
  sp_static = (vector int) { a, a, a, a };
}

void
splat_global (int a)
{
  sp_global = (vector int) { a, a, a, a };
}

int main (void)
{
  vector int sv2, sv3;
  int mem = SPLAT;
  int mem2[4] = { ELEMENTS };

  check (sv);

  check (pack_reg (ELEMENTS));

  check (pack_from_ptr (&mem2[0], &mem2[1], &mem2[2], &mem2[3]));

  check (pack_const ());

  pack_ptr (&sv2, ELEMENTS);
  check (sv2);

  pack_static (ELEMENTS);
  check (sv_static);

  pack_global (ELEMENTS);
  check (sv_global);

  check_splat (splat);

  check_splat (splat_reg (SPLAT));

  check_splat (splat_from_ptr (&mem));

  check_splat (splat_const ());

  splat_ptr (&sv2, SPLAT);
  check_splat (sv2);

  splat_static (SPLAT);
  check_splat (sp_static);

  splat_global (SPLAT);
  check_splat (sp_global);

  return 0;
}
