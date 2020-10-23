/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS -12345.0, 23456.0
#define SPLAT 34567.0

vector double sv = (vector double) { ELEMENTS };
vector double splat = (vector double) { SPLAT, SPLAT };
vector double sv_global, sp_global;
static vector double sv_static, sp_static;
static const int expected[] = { ELEMENTS };

extern void check (vector double a)
  __attribute__((__noinline__));

extern void check_splat (vector double a)
  __attribute__((__noinline__));

extern vector double pack_reg (double a, double b)
  __attribute__((__noinline__));

extern vector double pack_from_ptr (double *p_a, double *p_b)
  __attribute__((__noinline__));

extern vector double pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector double *p, double a, double b)
  __attribute__((__noinline__));

extern void pack_static (double a, double b)
  __attribute__((__noinline__));

extern void pack_global (double a, double b)
  __attribute__((__noinline__));

extern vector double splat_reg (double a)
  __attribute__((__noinline__));

extern vector double splat_from_ptr (double *p)
  __attribute__((__noinline__));

extern vector double splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector double *p, double a)
  __attribute__((__noinline__));

extern void splat_static (double a)
  __attribute__((__noinline__));

extern void splat_global (double a)
  __attribute__((__noinline__));

void
check (vector double a)
{
  size_t i;

  for (i = 0; i < 2; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector double a)
{
  size_t i;

  for (i = 0; i < 2; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector double
pack_reg (double a, double b)
{
  return (vector double) { a, b };
}

vector double
pack_from_ptr (double *p_a, double *p_b)
{
  return (vector double) { *p_a, *p_b };
}

vector double
pack_const (void)
{
  return (vector double) { ELEMENTS };
}

void
pack_ptr (vector double *p, double a, double b)
{
  *p = (vector double) { a, b };
}

void
pack_static (double a, double b)
{
  sv_static = (vector double) { a, b };
}

void
pack_global (double a, double b)
{
  sv_global = (vector double) { a, b };
}

vector double
splat_reg (double a)
{
  return (vector double) { a, a };
}

vector double
splat_from_ptr (double *p)
{
  return (vector double) { *p, *p };
}

vector double
splat_const (void)
{
  return (vector double) { SPLAT, SPLAT };
}

void
splat_ptr (vector double *p, double a)
{
  *p = (vector double) { a, a };
}

void
splat_static (double a)
{
  sp_static = (vector double) { a, a };
}

void
splat_global (double a)
{
  sp_global = (vector double) { a, a };
}

int  main (void)
{
  vector double sv2, sv3;
  double mem = SPLAT;
  double mem2[2] = { ELEMENTS };

  check (sv);

  check (pack_reg (ELEMENTS));

  check (pack_from_ptr (&mem2[0], &mem2[1]));

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
