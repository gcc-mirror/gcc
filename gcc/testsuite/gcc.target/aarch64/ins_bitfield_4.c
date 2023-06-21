/* { dg-do assemble } */
/* { dg-options "-O2 -mbig-endian --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

struct di_qi_1 { unsigned char c[4]; unsigned int x; };
struct di_qi_2 { unsigned int x; unsigned char c[4]; };

struct di_hi_1 { unsigned short s[2]; unsigned int x; };
struct di_hi_2 { unsigned int x; unsigned short s[2]; };

struct di_si { unsigned int i[2]; };

struct si_qi_1 { unsigned char c[2]; unsigned short x; };
struct si_qi_2 { unsigned short x; unsigned char c[2]; };

struct si_hi { unsigned short s[2]; };

#define TEST(NAME, STYPE, ETYPE, LHS)		\
  void						\
  NAME (volatile ETYPE *ptr)			\
  {						\
    register struct STYPE y asm ("v1");		\
    asm volatile ("" : "=w" (y));		\
    ETYPE x = *ptr;				\
    __UINT64_TYPE__ value = (ETYPE) x;		\
    LHS = value;				\
    asm volatile ("" :: "w" (y));		\
  }

/*
** f_di_qi_0:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[7\], v\1\.b\[0\]
**	ret
*/
TEST (f_di_qi_0, di_qi_1, unsigned char, y.c[0])

/*
** f_di_qi_1:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[4\], v\1\.b\[0\]
**	ret
*/
TEST (f_di_qi_1, di_qi_1, unsigned char, y.c[3])

/*
** f_di_qi_2:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[3\], v\1\.b\[0\]
**	ret
*/
TEST (f_di_qi_2, di_qi_2, unsigned char, y.c[0])

/*
** f_di_qi_3:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[0\], v\1\.b\[0\]
**	ret
*/
TEST (f_di_qi_3, di_qi_2, unsigned char, y.c[3])

/*
** f_di_hi_0:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[3\], v\1\.h\[0\]
**	ret
*/
TEST (f_di_hi_0, di_hi_1, unsigned short, y.s[0])

/*
** f_di_hi_1:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[2\], v\1\.h\[0\]
**	ret
*/
TEST (f_di_hi_1, di_hi_1, unsigned short, y.s[1])

/*
** f_di_hi_2:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[1\], v\1\.h\[0\]
**	ret
*/
TEST (f_di_hi_2, di_hi_2, unsigned short, y.s[0])

/*
** f_di_hi_3:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[0\], v\1\.h\[0\]
**	ret
*/
TEST (f_di_hi_3, di_hi_2, unsigned short, y.s[1])

/*
** f_di_si_0:
**	ldr	s([0-9]+), \[x0\]
**	ins	v1\.s\[1\], v\1\.s\[0\]
**	ret
*/
TEST (f_di_si_0, di_si, unsigned int, y.i[0])

/*
** f_di_si_1:
**	ldr	s([0-9]+), \[x0\]
**	ins	v1\.s\[0\], v\1\.s\[0\]
**	ret
*/
TEST (f_di_si_1, di_si, unsigned int, y.i[1])

/*
** f_si_qi_0:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[3\], v\1\.b\[0\]
**	ret
*/
TEST (f_si_qi_0, si_qi_1, unsigned char, y.c[0])

/*
** f_si_qi_1:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[2\], v\1\.b\[0\]
**	ret
*/
TEST (f_si_qi_1, si_qi_1, unsigned char, y.c[1])

/*
** f_si_qi_2:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[1\], v\1\.b\[0\]
**	ret
*/
TEST (f_si_qi_2, si_qi_2, unsigned char, y.c[0])

/*
** f_si_qi_3:
**	ldr	b([0-9]+), \[x0\]
**	ins	v1\.b\[0\], v\1\.b\[0\]
**	ret
*/
TEST (f_si_qi_3, si_qi_2, unsigned char, y.c[1])

/*
** f_si_hi_0:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[1\], v\1\.h\[0\]
**	ret
*/
TEST (f_si_hi_0, si_hi, unsigned short, y.s[0])

/*
** f_si_hi_1:
**	ldr	h([0-9]+), \[x0\]
**	ins	v1\.h\[0\], v\1\.h\[0\]
**	ret
*/
TEST (f_si_hi_1, si_hi, unsigned short, y.s[1])
