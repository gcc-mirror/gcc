/* { dg-do assemble } */
/* { dg-options "-O2 -mlittle-endian --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

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
  NAME (void)					\
  {						\
    register struct STYPE y asm ("v1");		\
    register ETYPE x asm ("x0");		\
    asm volatile ("" : "=w" (y), "=r" (x));	\
    LHS = x;					\
    asm volatile ("" :: "w" (y));		\
  }

/*
** f_di_qi_0:
**	ins	v1\.b\[0\], w0
**	ret
*/
TEST (f_di_qi_0, di_qi_1, unsigned char, y.c[0])

/*
** f_di_qi_1:
**	ins	v1\.b\[3\], w0
**	ret
*/
TEST (f_di_qi_1, di_qi_1, unsigned char, y.c[3])

/*
** f_di_qi_2:
**	ins	v1\.b\[4\], w0
**	ret
*/
TEST (f_di_qi_2, di_qi_2, unsigned char, y.c[0])

/*
** f_di_qi_3:
**	ins	v1\.b\[7\], w0
**	ret
*/
TEST (f_di_qi_3, di_qi_2, unsigned char, y.c[3])

/*
** f_di_hi_0:
**	ins	v1\.h\[0\], w0
**	ret
*/
TEST (f_di_hi_0, di_hi_1, unsigned short, y.s[0])

/*
** f_di_hi_1:
**	ins	v1\.h\[1\], w0
**	ret
*/
TEST (f_di_hi_1, di_hi_1, unsigned short, y.s[1])

/*
** f_di_hi_2:
**	ins	v1\.h\[2\], w0
**	ret
*/
TEST (f_di_hi_2, di_hi_2, unsigned short, y.s[0])

/*
** f_di_hi_3:
**	ins	v1\.h\[3\], w0
**	ret
*/
TEST (f_di_hi_3, di_hi_2, unsigned short, y.s[1])

/*
** f_di_si_0:
**	ins	v1\.s\[0\], w0
**	ret
*/
TEST (f_di_si_0, di_si, unsigned int, y.i[0])

/*
** f_di_si_1:
**	ins	v1\.s\[1\], w0
**	ret
*/
TEST (f_di_si_1, di_si, unsigned int, y.i[1])

/*
** f_si_qi_0:
**	ins	v1\.b\[0\], w0
**	ret
*/
TEST (f_si_qi_0, si_qi_1, unsigned char, y.c[0])

/*
** f_si_qi_1:
**	ins	v1\.b\[1\], w0
**	ret
*/
TEST (f_si_qi_1, si_qi_1, unsigned char, y.c[1])

/*
** f_si_qi_2:
**	ins	v1\.b\[2\], w0
**	ret
*/
TEST (f_si_qi_2, si_qi_2, unsigned char, y.c[0])

/*
** f_si_qi_3:
**	ins	v1\.b\[3\], w0
**	ret
*/
TEST (f_si_qi_3, si_qi_2, unsigned char, y.c[1])

/*
** f_si_hi_0:
**	ins	v1\.h\[0\], w0
**	ret
*/
TEST (f_si_hi_0, si_hi, unsigned short, y.s[0])

/*
** f_si_hi_1:
**	ins	v1\.h\[1\], w0
**	ret
*/
TEST (f_si_hi_1, si_hi, unsigned short, y.s[1])
