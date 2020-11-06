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

#define PERM0(B) B, B + 1
#define PERM1(B) PERM0 (B), PERM0 (B + 2)
#define PERM2(B) PERM1 (B), PERM1 (B + 4)
#define PERM3(B) PERM2 (B), PERM2 (B + 8)
#define PERM4(B) PERM3 (B), PERM3 (B + 16)
#define PERM5(B) PERM4 (B), PERM4 (B + 32)
#define PERM6(B) PERM5 (B), PERM5 (B + 64)

/*
** qi_ext_h_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #2
**	st1b	\2\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_ext_h_1 (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (1) });
}

/*
** qi_ext_h_1_two_op:
**	ptrue	(p[0-7])\.b, vl256
** (
**	ld1b	(z[0-9]+)\.h, \1/z, \[x1\]
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	ext	\3\.b, \3\.b, \2\.b, #2
**	st1b	\3\.h, \1, \[x8\]
** |
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	ld1b	(z[0-9]+)\.h, \1/z, \[x1\]
**	ext	\4\.b, \4\.b, \5\.b, #2
**	st1b	\4\.h, \1, \[x8\]
** )
**	ret
*/
v128qi
qi_ext_h_1_two_op (v128qi x, v128qi y)
{
  return __builtin_shuffle (x, y, (v128qi) { PERM6 (1) });
}

/*
** qi_ext_h_127:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.h, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #254
**	st1b	\2\.h, \1, \[x8\]
**	ret
*/
v128qi
qi_ext_h_127 (v128qi x)
{
  return __builtin_shuffle (x, x, (v128qi) { PERM6 (127) });
}

/*
** qi_ext_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #4
**	st1b	\2\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_ext_s_1 (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (1) });
}

/*
** qi_ext_s_63:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #252
**	st1b	\2\.s, \1, \[x8\]
**	ret
*/
v64qi
qi_ext_s_63 (v64qi x)
{
  return __builtin_shuffle (x, x, (v64qi) { PERM5 (63) });
}

/*
** qi_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1b	\2\.d, \1, \[x8\]
**	ret
*/
v32qi
qi_ext_d_1 (v32qi x)
{
  return __builtin_shuffle (x, x, (v32qi) { PERM4 (1) });
}

/*
** qi_ext_d_31:
**	ptrue	(p[0-7])\.b, vl256
**	ld1b	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #248
**	st1b	\2\.d, \1, \[x8\]
**	ret
*/
v32qi
qi_ext_d_31 (v32qi x)
{
  return __builtin_shuffle (x, x, (v32qi) { PERM4 (31) });
}

/*
** hi_ext_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #4
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_ext_s_1 (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** hi_ext_s_63:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #252
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64hi
hi_ext_s_63 (v64hi x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (63) });
}

/*
** hf_ext_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #4
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_ext_s_1 (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** hf_ext_s_60:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #240
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64hf
hf_ext_s_60 (v64hf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (60) });
}

/*
** bf_ext_s_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #4
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_ext_s_1 (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (1) });
}

/*
** bf_ext_s_40:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #160
**	st1h	\2\.s, \1, \[x8\]
**	ret
*/
v64bf
bf_ext_s_40 (v64bf x)
{
  return __builtin_shuffle (x, x, (v64hi) { PERM5 (40) });
}

/*
** hi_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32hi
hi_ext_d_1 (v32hi x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** hi_ext_d_31:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #248
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32hi
hi_ext_d_31 (v32hi x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (31) });
}

/*
** hf_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32hf
hf_ext_d_1 (v32hf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** hf_ext_d_18:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #144
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32hf
hf_ext_d_18 (v32hf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (18) });
}

/*
** bf_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32bf
bf_ext_d_1 (v32bf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (1) });
}

/*
** bf_ext_d_7:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #56
**	st1h	\2\.d, \1, \[x8\]
**	ret
*/
v32bf
bf_ext_d_7 (v32bf x)
{
  return __builtin_shuffle (x, x, (v32hi) { PERM4 (7) });
}

/*
** si_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1w	\2\.d, \1, \[x8\]
**	ret
*/
v32si
si_ext_d_1 (v32si x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (1) });
}

/*
** si_ext_d_31:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #248
**	st1w	\2\.d, \1, \[x8\]
**	ret
*/
v32si
si_ext_d_31 (v32si x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (31) });
}

/*
** sf_ext_d_1:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #8
**	st1w	\2\.d, \1, \[x8\]
**	ret
*/
v32sf
sf_ext_d_1 (v32sf x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (1) });
}

/*
** sf_ext_d_31:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	ext	\2\.b, \2\.b, \2\.b, #248
**	st1w	\2\.d, \1, \[x8\]
**	ret
*/
v32sf
sf_ext_d_31 (v32sf x)
{
  return __builtin_shuffle (x, x, (v32si) { PERM4 (31) });
}
