/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#define ELEMENTS 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 127, -1, -128
#define SPLAT 0x12

vector signed char sv = (vector signed char) { ELEMENTS };
vector signed char splat = (vector signed char) { SPLAT, SPLAT, SPLAT, SPLAT,
						  SPLAT, SPLAT, SPLAT, SPLAT,
						  SPLAT, SPLAT, SPLAT, SPLAT,
						  SPLAT, SPLAT, SPLAT, SPLAT };
vector signed char sv_global, sp_global;
static vector signed char sv_static, sp_static;
static signed char expected[] = { ELEMENTS };
static signed char splat_expected = SPLAT;

extern void check (vector signed char a)
  __attribute__((__noinline__));

extern void check_splat (vector signed char a)
  __attribute__((__noinline__));

extern vector signed char pack_reg (signed char a, signed char b,
				    signed char c, signed char d,
				    signed char e, signed char f,
				    signed char g, signed char h,
				    signed char i, signed char j,
				    signed char k, signed char l,
				    signed char m, signed char n,
				    signed char o, signed char p)
  __attribute__((__noinline__));

extern vector signed char pack_from_ptr (signed char *p_a, signed char *p_b,
					 signed char *p_c, signed char *p_d,
					 signed char *p_e, signed char *p_f,
					 signed char *p_g, signed char *p_h,
					 signed char *p_i, signed char *p_j,
					 signed char *p_k, signed char *p_l,
					 signed char *p_m, signed char *p_n,
					 signed char *p_o, signed char *p_p)
  __attribute__((__noinline__));

extern vector signed char pack_const (void)
  __attribute__((__noinline__));

extern void pack_ptr (vector signed char *q,
		      signed char a, signed char b, signed char c, signed char d,
		      signed char e, signed char f, signed char g, signed char h,
		      signed char i, signed char j, signed char k, signed char l,
		      signed char m, signed char n, signed char o, signed char p)
  __attribute__((__noinline__));

extern void pack_static (signed char a, signed char b, signed char c, signed char d,
			 signed char e, signed char f, signed char g, signed char h,
			 signed char i, signed char j, signed char k, signed char l,
			 signed char m, signed char n, signed char o, signed char p)
  __attribute__((__noinline__));

extern void pack_global (signed char a, signed char b, signed char c, signed char d,
			 signed char e, signed char f, signed char g, signed char h,
			 signed char i, signed char j, signed char k, signed char l,
			 signed char m, signed char n, signed char o, signed char p)
  __attribute__((__noinline__));

extern vector signed char splat_reg (signed char a)
  __attribute__((__noinline__));

extern vector signed char splat_from_ptr (signed char *p_a)
  __attribute__((__noinline__));

extern vector signed char splat_const (void)
  __attribute__((__noinline__));

extern void splat_ptr (vector signed char *p, signed char a)
  __attribute__((__noinline__));

extern void splat_static (signed char a)
  __attribute__((__noinline__));

extern void splat_global (signed char a)
  __attribute__((__noinline__));

void
check (vector signed char a)
{
  size_t i;

  for (i = 0; i < 16; i++)
    if (vec_extract (a, i) != expected[i])
      abort ();
}

void
check_splat (vector signed char a)
{
  size_t i;

  for (i = 0; i < 16; i++)
    if (vec_extract (a, i) != SPLAT)
      abort ();
}

vector signed char
pack_reg (signed char a, signed char b, signed char c, signed char d,
	  signed char e, signed char f, signed char g, signed char h,
	  signed char i, signed char j, signed char k, signed char l,
	  signed char m, signed char n, signed char o, signed char p)
{
  return (vector signed char) { a, b, c, d, e, f, g, h,
				i, j, k, l, m, n, o, p };
}

vector signed char
pack_from_ptr (signed char *p_a, signed char *p_b, signed char *p_c, signed char *p_d,
	       signed char *p_e, signed char *p_f, signed char *p_g, signed char *p_h,
	       signed char *p_i, signed char *p_j, signed char *p_k, signed char *p_l,
	       signed char *p_m, signed char *p_n, signed char *p_o, signed char *p_p)
{
  return (vector signed char) { *p_a, *p_b, *p_c, *p_d,
				*p_e, *p_f, *p_g, *p_h,
				*p_i, *p_j, *p_k, *p_l,
				*p_m, *p_n, *p_o, *p_p };

}

vector signed char
pack_const (void)
{
  return (vector signed char) { ELEMENTS };
}

void
pack_ptr (vector signed char *q,
	  signed char a, signed char b, signed char c, signed char d,
	  signed char e, signed char f, signed char g, signed char h,
	  signed char i, signed char j, signed char k, signed char l,
	  signed char m, signed char n, signed char o, signed char p)
{
  *q = (vector signed char) { a, b, c, d, e, f, g, h,
			      i, j, k, l, m, n, o, p };
}

void
pack_static (signed char a, signed char b, signed char c, signed char d,
	     signed char e, signed char f, signed char g, signed char h,
	     signed char i, signed char j, signed char k, signed char l,
	     signed char m, signed char n, signed char o, signed char p)
{
  sv_static = (vector signed char) { a, b, c, d, e, f, g, h,
				     i, j, k, l, m, n, o, p };
}

void
pack_global (signed char a, signed char b, signed char c, signed char d,
	     signed char e, signed char f, signed char g, signed char h,
	     signed char i, signed char j, signed char k, signed char l,
	     signed char m, signed char n, signed char o, signed char p)
{
  sv_global = (vector signed char) { a, b, c, d, e, f, g, h,
				     i, j, k, l, m, n, o, p };
}

vector signed char
splat_reg (signed char a)
{
  return (vector signed char) { a, a, a, a, a, a, a, a,
				a, a, a, a, a, a, a, a };
}

vector signed char
splat_from_ptr (signed char *p_a)
{
  return (vector signed char) { *p_a, *p_a, *p_a, *p_a,
				*p_a, *p_a, *p_a, *p_a,
				*p_a, *p_a, *p_a, *p_a,
				*p_a, *p_a, *p_a, *p_a };
}

vector signed char
splat_const (void)
{
  return (vector signed char) { SPLAT, SPLAT, SPLAT, SPLAT,
				SPLAT, SPLAT, SPLAT, SPLAT,
				SPLAT, SPLAT, SPLAT, SPLAT,
				SPLAT, SPLAT, SPLAT, SPLAT };
}

void
splat_ptr (vector signed char *p, signed char a)
{
  *p = (vector signed char) { a, a, a, a, a, a, a, a,
			      a, a, a, a, a, a, a, a };
}

void
splat_static (signed char a)
{
  sp_static = (vector signed char) { a, a, a, a, a, a, a, a,
				     a, a, a, a, a, a, a, a };
}

void
splat_global (signed char a)
{
  sp_global = (vector signed char) { a, a, a, a, a, a, a, a,
				     a, a, a, a, a, a, a, a };
}

int main (void)
{
  vector signed char sv2, sv3;

  check (sv);

  check (pack_reg (ELEMENTS));

  check (pack_from_ptr (&expected[0],  &expected[1],  &expected[2],
			&expected[3],  &expected[4],  &expected[5],
			&expected[6],  &expected[7],  &expected[8],
			&expected[9],  &expected[10], &expected[11],
			&expected[12], &expected[13], &expected[14],
			&expected[15]));

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
