/* { dg-do run { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS -12345678L, 9L
#define SPLAT 0x0123456789ABCDE

vector long sv = (vector long) { ELEMENTS };
vector long splat = (vector long) { SPLAT, SPLAT };
vector long sv_global, sp_global;
static vector long sv_static, sp_static;
static const int expected[] = { ELEMENTS };

extern void check (vector long a)
  __attribute__((__noinline__));

extern void check_splat (vector long a)
  __attribute__((__noinline__));

extern vector long pack_reg (long a, long b)
  __attribute__((__noinline__));

extern vector long pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector long *p, long a, long b)
  __attribute__((__noinline__));

extern void pack_static (long a, long b)
  __attribute__((__noinline__));

extern void pack_global (long a, long b)
  __attribute__((__noinline__));

extern vector long splat_reg (long a)
  __attribute__((__noinline__));

extern vector long splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector long *p, long a)
  __attribute__((__noinline__));

extern void splat_static (long a)
  __attribute__((__noinline__));

extern void splat_global (long a)
  __attribute__((__noinline__));

void
check (vector long a)
{
  size_t i;

  for (i = 0; i < 2; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector long a)
{
  size_t i;

  for (i = 0; i < 2; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector long
pack_reg (long a, long b)
{
  return (vector long) { a, b };
}

vector long
pack_const (void)
{
  return (vector long) { ELEMENTS };
}

void
pack_ptr (vector long *p, long a, long b)
{
  *p = (vector long) { a, b };
}

void
pack_static (long a, long b)
{
  sv_static = (vector long) { a, b };
}

void
pack_global (long a, long b)
{
  sv_global = (vector long) { a, b };
}

vector long
splat_reg (long a)
{
  return (vector long) { a, a };
}

vector long
splat_const (void)
{
  return (vector long) { SPLAT, SPLAT };
}

void
splat_ptr (vector long *p, long a)
{
  *p = (vector long) { a, a };
}

void
splat_static (long a)
{
  sp_static = (vector long) { a, a };
}

void
splat_global (long a)
{
  sp_global = (vector long) { a, a };
}

int  main (void)
{
  vector long sv2, sv3;

  check (sv);

  check (pack_reg (ELEMENTS));

  check (pack_const ());

  pack_ptr (&sv2, ELEMENTS);
  check (sv2);

  pack_static (ELEMENTS);
  check (sv_static);

  pack_global (ELEMENTS);
  check (sv_global);

  check_splat (splat);

  check_splat (splat_reg (SPLAT));

  check_splat (splat_const ());

  splat_ptr (&sv2, SPLAT);
  check_splat (sv2);

  splat_static (SPLAT);
  check_splat (sp_static);

  splat_global (SPLAT);
  check_splat (sp_global);

  return 0;
}
