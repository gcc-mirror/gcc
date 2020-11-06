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

#define PERM0(B) B, B
#define PERM1(B) PERM0 (B), PERM0 (B)
#define PERM2(B) PERM1 (B), PERM1 (B)
#define PERM3(B) PERM2 (B), PERM2 (B)
#define PERM4(B) PERM3 (B), PERM3 (B)
#define PERM5(B) PERM4 (B), PERM4 (B)
#define PERM6(B) PERM5 (B), PERM5 (B)

/*
** qi_dup_h_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	dup	(z[0-9]+)\.h, \2\.h\[1\]
**	st1b	\3\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_dup_h_1 (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (1) });
}

/*
** qi_dup_h_31:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	dup	(z[0-9]+)\.h, \2\.h\[31\]
**	st1b	\3\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_dup_h_31 (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (31) });
}

/*
** qi_dup_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[1\]
**	st1b	\3\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_dup_s_1 (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (1) });
}

/*
** qi_dup_s_15:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[15\]
**	st1b	\3\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_dup_s_15 (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (15) });
}

/*
** qi_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1b	\3\.d, \1, \[x8\]
**	ret
*/
v32qi
qi_dup_d_1 (v32qi x)
{
  return __builtin_shuffle (x, x, (v32qi) { PERM4 (1) });
}

/*
** qi_dup_d_7:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[7\]
**	st1b	\3\.d, \1, \[x8\]
**	ret
*/
v32qi
qi_dup_d_7 (v32qi x)
{
  return __builtin_shuffle (x, x, (v32qi) { PERM4 (7) });
}

/*
** hi_dup_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[1\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_dup_s_1 (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** hi_dup_s_15:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[15\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_dup_s_15 (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (15) });
}

/*
** hf_dup_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[1\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_dup_s_1 (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** hf_dup_s_11:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[11\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_dup_s_11 (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (11) });
}

/*
** bf_dup_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[1\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_dup_s_1 (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** bf_dup_s_13:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	dup	(z[0-9]+)\.s, \2\.s\[13\]
**	st1h	\3\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_dup_s_13 (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (13) });
}

/*
** hi_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hi
hi_dup_d_1 (v32hi x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** hi_dup_d_7:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[7\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hi
hi_dup_d_7 (v32hi x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (7) });
}

/*
** hf_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hf
hf_dup_d_1 (v32hf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** hf_dup_d_5:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[5\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32hf
hf_dup_d_5 (v32hf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (5) });
}

/*
** bf_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32bf
bf_dup_d_1 (v32bf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** bf_dup_d_6:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[6\]
**	st1h	\3\.d, \1, \[x8\]
**	ret
*/
v32bf
bf_dup_d_6 (v32bf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (6) });
}

/*
** si_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32si
si_dup_d_1 (v32si x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (1) });
}

/*
** si_dup_d_7:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[7\]
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32si
si_dup_d_7 (v32si x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (7) });
}

/*
** sf_dup_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[1\]
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32sf
sf_dup_d_1 (v32sf x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (1) });
}

/*
** sf_dup_d_7:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	dup	(z[0-9]+)\.d, \2\.d\[7\]
**	st1w	\3\.d, \1, \[x8\]
**	ret
*/
v32sf
sf_dup_d_7 (v32sf x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (7) });
}
