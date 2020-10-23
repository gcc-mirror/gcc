/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS -1.0f, 2.0f, 0.0f, -1234.0f
#define SPLAT 2345.0f

vector float sv = (vector float) { ELEMENTS };
vector float splat = (vector float) { SPLAT, SPLAT, SPLAT, SPLAT };
vector float sv_global, sp_global;
static vector float sv_static, sp_static;
static const float expected[] = { ELEMENTS };

extern void check (vector float a)
  __attribute__((__noinline__));

extern void check_splat (vector float a)
  __attribute__((__noinline__));

extern vector float pack_reg (float a, float b, float c, float d)
  __attribute__((__noinline__));

extern vector float pack_from_ptr (float *p_a, float *p_b,
				   float *p_c, float *p_d)
  __attribute__((__noinline__));

extern vector float pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector float *p, float a, float b, float c, float d)
  __attribute__((__noinline__));

extern void pack_static (float a, float b, float c, float d)
  __attribute__((__noinline__));

extern void pack_global (float a, float b, float c, float d)
  __attribute__((__noinline__));

extern vector float splat_reg (float a)
  __attribute__((__noinline__));

extern vector float splat_from_ptr (float *p)
  __attribute__((__noinline__));

extern vector float splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector float *p, float a)
  __attribute__((__noinline__));

extern void splat_static (float a)
  __attribute__((__noinline__));

extern void splat_global (float a)
  __attribute__((__noinline__));

void
check (vector float a)
{
  size_t i;

  for (i = 0; i < 4; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector float a)
{
  size_t i;

  for (i = 0; i < 4; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector float
pack_reg (float a, float b, float c, float d)
{
  return (vector float) { a, b, c, d };
}

vector float
pack_from_ptr (float *p_a, float *p_b, float *p_c, float *p_d)
{
  return (vector float) { *p_a, *p_b, *p_c, *p_d };
}

vector float
pack_const (void)
{
  return (vector float) { ELEMENTS };
}

void
pack_ptr (vector float *p, float a, float b, float c, float d)
{
  *p = (vector float) { a, b, c, d };
}

void
pack_static (float a, float b, float c, float d)
{
  sv_static = (vector float) { a, b, c, d };
}

void
pack_global (float a, float b, float c, float d)
{
  sv_global = (vector float) { a, b, c, d };
}

vector float
splat_reg (float a)
{
  return (vector float) { a, a, a, a };
}

vector float
splat_from_ptr (float *p)
{
  return (vector float) { *p, *p, *p, *p };
}

vector float
splat_const (void)
{
  return (vector float) { SPLAT, SPLAT, SPLAT, SPLAT };
}

void
splat_ptr (vector float *p, float a)
{
  *p = (vector float) { a, a, a, a };
}

void
splat_static (float a)
{
  sp_static = (vector float) { a, a, a, a };
}

void
splat_global (float a)
{
  sp_global = (vector float) { a, a, a, a };
}

int main (void)
{
  vector float sv2, sv3;
  float mem = SPLAT;
  float mem2[4] = { ELEMENTS };

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
