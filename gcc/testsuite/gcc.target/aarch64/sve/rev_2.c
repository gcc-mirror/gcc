/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -mlittle-endian --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned char v128qi __attribute__((vector_size(128)));
typedef unsigned char v64qi __attribute__((vector_size(64)));
typedef unsigned char v32qi __attribute__((vector_size(32)));
typedef unsigned short v64hi __attribute__((vector_size(128)));
typedef unsigned short v32hi __attribute__((vector_size(64)));
typedef _Float16 v64hf __attribute__((vector_size(128)));
typedef _Float16 v32hf __attribute__((vector_size(64)));
typedef __bf16 v64bf __attribute__((vector_size(128)));
typedef __bf16 v32bf __attribute__((vector_size(64)));
typedef unsigned int v32si __attribute__((vector_size(128)));
typedef float v32sf __attribute__((vector_size(128)));

#define PERM0(B) B, B - 1
#define PERM1(B) PERM0 (B), PERM0 (B - 2)
#define PERM2(B) PERM1 (B), PERM1 (B - 4)
#define PERM3(B) PERM2 (B), PERM2 (B - 8)
#define PERM4(B) PERM3 (B), PERM3 (B - 16)
#define PERM5(B) PERM4 (B), PERM4 (B - 32)
#define PERM6(B) PERM5 (B), PERM5 (B - 64)

/*
** qi_rev_h:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	rev	(z[0-9]+)\.h, \2\.h
**	st1b	\3\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_rev_h (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (127) });
}

/*
** qi_rev_s:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	rev	(z[0-9]+)\.s, \2\.s
**	st1b	\3\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_rev_s (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (63) });
}

/*
** qi_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1b	\3\.d, \1, \[x8\]
**	ret
*/
v32qi
qi_rev_d (v32qi x)
{
  return __builtin_shuffle (x, x, (v32qi) { PERM4 (31) });
}

/*
** hi_rev_s:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	rev	(z[0-9]+)\.s, \2\.s
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_rev_s (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (63) });
}

/*
** hf_rev_s:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	rev	(z[0-9]+)\.s, \2\.s
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_rev_s (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (63) });
}

/*
** bf_rev_s:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	rev	(z[0-9]+)\.s, \2\.s
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_rev_s (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (63) });
}

/*
** hi_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hi
hi_rev_d (v32hi x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (31) });
}

/*
** hf_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hf
hf_rev_d (v32hf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (31) });
}

/*
** bf_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32bf
bf_rev_d (v32bf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (31) });
}

/*
** si_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32si
si_rev_d (v32si x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (31) });
}

/*
** sf_rev_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	rev	(z[0-9]+)\.d, \2\.d
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32sf
sf_rev_d (v32sf x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (31) });
}
