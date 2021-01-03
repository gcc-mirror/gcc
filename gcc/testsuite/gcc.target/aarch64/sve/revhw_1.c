/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -mlittle-endian --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned char v128qi __attribute__((vector_size(128)));
typedef unsigned char v64qi __attribute__((vector_size(64)));
typedef unsigned short v64hi __attribute__((vector_size(128)));
typedef _Float16 v64hf __attribute__((vector_size(128)));
typedef __bf16 v64bf __attribute__((vector_size(128)));

#define PERM0(B) B + 1, B
#define PERM1(B) PERM0 (B), PERM0 (B + 2)
#define PERM2(B) PERM1 (B), PERM1 (B + 4)
#define PERM3(B) PERM2 (B), PERM2 (B + 8)
#define PERM4(B) PERM3 (B), PERM3 (B + 16)
#define PERM5(B) PERM4 (B), PERM4 (B + 32)
#define PERM6(B) PERM5 (B), PERM5 (B + 64)

/*
** qi_revh_s:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	revh	(z[0-9]+)\.s, \1/m, \2\.s
**	st1b	\3\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_revh_s (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (0) });
}

/*
** qi_revw_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	revw	(z[0-9]+)\.d, \1/m, \2\.d
**	st1b	\3\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_revw_d (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (0) });
}

/*
** hi_revw_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	revw	(z[0-9]+)\.d, \1/m, \2\.d
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_revw_d (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (0) });
}

/*
** hf_revw_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	revw	(z[0-9]+)\.d, \1/m, \2\.d
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_revw_d (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (0) });
}

/*
** bf_revw_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	revw	(z[0-9]+)\.d, \1/m, \2\.d
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_revw_d (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (0) });
}

#undef PERM1
#define PERM1(B) PERM0 (B + 2), PERM0 (B)

/*
** qi_revh_d:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	revh	(z[0-9]+)\.d, \1/m, \2\.d
**	st1b	\3\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_revh_d (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (0) });
}

v64qi
qi_revw_q (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (0) });
}

v64hi
hi_revw_q (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (0) });
}

#undef PERM2
#define PERM2(B) PERM0 (B + 4), PERM0 (B)

v128qi
qi_revh_q (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (0) });
}

/* { dg-final { scan-assembler-times {\trev.\t} 6 } } */
