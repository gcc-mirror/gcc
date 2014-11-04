/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O3 -mcpu=power7 -Wno-deprecated" } */

/* Test the various load/store varients.  */

#include <altivec.h>

#define TEST_COPY(NAME, TYPE)						\
void NAME ## _copy_native (vector TYPE *a, vector TYPE *b)		\
{									\
  *a = *b;								\
}									\
									\
void NAME ## _copy_vec (vector TYPE *a, vector TYPE *b)			\
{									\
  vector TYPE x = vec_ld (0, b);					\
  vec_st (x, 0, a);							\
}									\

#define TEST_COPYL(NAME, TYPE)						\
void NAME ## _lvxl (vector TYPE *a, vector TYPE *b)			\
{									\
  vector TYPE x = vec_ldl (0, b);					\
  vec_stl (x, 0, a);							\
}									\

#define TEST_VSX_COPY(NAME, TYPE)					\
void NAME ## _copy_vsx (vector TYPE *a, vector TYPE *b)			\
{									\
  vector TYPE x = vec_vsx_ld (0, b);					\
  vec_vsx_st (x, 0, a);							\
}									\

#define TEST_ALIGN(NAME, TYPE)						\
void NAME ## _align (vector unsigned char *a, TYPE *b)			\
{									\
  vector unsigned char x = vec_lvsl (0, b);				\
  vector unsigned char y = vec_lvsr (0, b);				\
  vec_st (x, 0, a);							\
  vec_st (y, 8, a);							\
}

#ifndef NO_COPY
TEST_COPY(uchar,  unsigned char)
TEST_COPY(schar,  signed   char)
TEST_COPY(bchar,  bool     char)
TEST_COPY(ushort, unsigned short)
TEST_COPY(sshort, signed   short)
TEST_COPY(bshort, bool     short)
TEST_COPY(uint,   unsigned int)
TEST_COPY(sint,   signed   int)
TEST_COPY(bint,   bool     int)
TEST_COPY(float,  float)
TEST_COPY(double, double)
#endif	/* NO_COPY */

#ifndef NO_COPYL
TEST_COPYL(uchar,  unsigned char)
TEST_COPYL(schar,  signed   char)
TEST_COPYL(bchar,  bool     char)
TEST_COPYL(ushort, unsigned short)
TEST_COPYL(sshort, signed   short)
TEST_COPYL(bshort, bool     short)
TEST_COPYL(uint,   unsigned int)
TEST_COPYL(sint,   signed   int)
TEST_COPYL(bint,   bool     int)
TEST_COPYL(float,  float)
TEST_COPYL(double, double)
#endif	/* NO_COPYL */

#ifndef NO_ALIGN
TEST_ALIGN(uchar,  unsigned char)
TEST_ALIGN(schar,  signed   char)
TEST_ALIGN(ushort, unsigned short)
TEST_ALIGN(sshort, signed   short)
TEST_ALIGN(uint,   unsigned int)
TEST_ALIGN(sint,   signed   int)
TEST_ALIGN(float,  float)
TEST_ALIGN(double, double)
#endif	/* NO_ALIGN */


#ifndef NO_VSX_COPY
TEST_VSX_COPY(uchar,  unsigned char)
TEST_VSX_COPY(schar,  signed   char)
TEST_VSX_COPY(bchar,  bool     char)
TEST_VSX_COPY(ushort, unsigned short)
TEST_VSX_COPY(sshort, signed   short)
TEST_VSX_COPY(bshort, bool     short)
TEST_VSX_COPY(uint,   unsigned int)
TEST_VSX_COPY(sint,   signed   int)
TEST_VSX_COPY(bint,   bool     int)
TEST_VSX_COPY(float,  float)
TEST_VSX_COPY(double, double)
#endif	/* NO_VSX_COPY */
