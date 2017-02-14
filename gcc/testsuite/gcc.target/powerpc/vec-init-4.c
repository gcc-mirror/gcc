/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS -1, 2, 0, -32768, 32767, 53, 1, 16000
#define SPLAT 0x0123

vector short sv = (vector short) { ELEMENTS };
vector short splat = (vector short) { SPLAT, SPLAT, SPLAT, SPLAT,
				      SPLAT, SPLAT, SPLAT, SPLAT };
vector short sv_global, sp_global;
static vector short sv_static, sp_static;
static short expected[] = { ELEMENTS };
static short splat_expected = SPLAT;

extern void check (vector short a)
  __attribute__((__noinline__));

extern void check_splat (vector short a)
  __attribute__((__noinline__));

extern vector short pack_reg (short a, short b, short c, short d,
			      short e, short f, short g, short h)
  __attribute__((__noinline__));

extern vector short pack_from_ptr (short *p_a, short *p_b,
				   short *p_c, short *p_d,
				   short *p_e, short *p_f,
				   short *p_g, short *p_h)
  __attribute__((__noinline__));

extern vector short pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector short *p,
		      short a, short b, short c, short d,
		      short e, short f, short g, short h)
  __attribute__((__noinline__));

extern void pack_static (short a, short b, short c, short d,
			 short e, short f, short g, short h)
  __attribute__((__noinline__));

extern void pack_global (short a, short b, short c, short d,
			 short e, short f, short g, short h)
  __attribute__((__noinline__));

extern vector short splat_reg (short a)
  __attribute__((__noinline__));

extern vector short splat_from_ptr (short *p_a)
  __attribute__((__noinline__));

extern vector short splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector short *p, short a)
  __attribute__((__noinline__));

extern void splat_static (short a)
  __attribute__((__noinline__));

extern void splat_global (short a)
  __attribute__((__noinline__));

void
check (vector short a)
{
  size_t i;

  for (i = 0; i < 8; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector short a)
{
  size_t i;

  for (i = 0; i < 8; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector short
pack_reg (short a, short b, short c, short d,
	  short e, short f, short g, short h)
{
  return (vector short) { a, b, c, d, e, f, g, h };
}

vector short
pack_from_ptr (short *p_a, short *p_b, short *p_c, short *p_d,
	       short *p_e, short *p_f, short *p_g, short *p_h)
{
  return (vector short) { *p_a, *p_b, *p_c, *p_d,
			  *p_e, *p_f, *p_g, *p_h };
}

vector short
pack_const (void)
{
  return (vector short) { ELEMENTS };
}

void
pack_ptr (vector short *p,
	  short a, short b, short c, short d,
	  short e, short f, short g, short h)
{
  *p = (vector short) { a, b, c, d, e, f, g, h };
}

void
pack_static (short a, short b, short c, short d,
	     short e, short f, short g, short h)
{
  sv_static = (vector short) { a, b, c, d, e, f, g, h };
}

void
pack_global (short a, short b, short c, short d,
	     short e, short f, short g, short h)
{
  sv_global = (vector short) { a, b, c, d, e, f, g, h };
}

vector short
splat_reg (short a)
{
  return (vector short) { a, a, a, a, a, a, a, a };
}

vector short
splat_from_ptr (short *p_a)
{
  return (vector short) { *p_a, *p_a, *p_a, *p_a,
			  *p_a, *p_a, *p_a, *p_a };
}

vector short
splat_const (void)
{
  return (vector short) { SPLAT, SPLAT, SPLAT, SPLAT,
			  SPLAT, SPLAT, SPLAT, SPLAT };
}

void
splat_ptr (vector short *p, short a)
{
  *p = (vector short) { a, a, a, a, a, a, a, a };
}

void
splat_static (short a)
{
  sp_static = (vector short) { a, a, a, a, a, a, a, a };
}

void
splat_global (short a)
{
  sp_global = (vector short) { a, a, a, a, a, a, a, a };
}

int main (void)
{
  vector short sv2, sv3;

  check (sv);

  check (pack_reg (ELEMENTS));

  check (pack_from_ptr (&expected[0], &expected[1], &expected[2],
			&expected[3], &expected[4], &expected[5],
			&expected[6], &expected[7]));

  check (pack_const ());

  pack_ptr (&sv2, ELEMENTS);
  check (sv2);

  pack_static (ELEMENTS);
  check (sv_static);

  pack_global (ELEMENTS);
  check (sv_global);

  check_splat (splat);

  check_splat (splat_reg (SPLAT));

  check_splat (splat_from_ptr (&splat_expected));

  check_splat (splat_const ());

  splat_ptr (&sv2, SPLAT);
  check_splat (sv2);

  splat_static (SPLAT);
  check_splat (sp_static);

  splat_global (SPLAT);
  check_splat (sp_global);

  return 0;
}
