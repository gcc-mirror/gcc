#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "fp2-struct-defs.h"
#include "fp2-struct-init.h"

#define TEST(TYPE)						\
extern TYPE g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE;		\
extern TYPE g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE;		\
extern TYPE g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE;	\
extern TYPE g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE;	\
								\
extern void check##TYPE (TYPE x, double y);			\
								\
void								\
checkg##TYPE (void)						\
{								\
  check##TYPE (  g1s##TYPE,  (double)1);			\
  check##TYPE (  g2s##TYPE,  (double)2);			\
  check##TYPE (  g3s##TYPE,  (double)3);			\
  check##TYPE (  g4s##TYPE,  (double)4);			\
  check##TYPE (  g5s##TYPE,  (double)5);			\
  check##TYPE (  g6s##TYPE,  (double)6);			\
  check##TYPE (  g7s##TYPE,  (double)7);			\
  check##TYPE (  g8s##TYPE,  (double)8);			\
  check##TYPE (  g9s##TYPE,  (double)9);			\
  check##TYPE ( g10s##TYPE, (double)10);			\
  check##TYPE ( g11s##TYPE, (double)11);			\
  check##TYPE ( g12s##TYPE, (double)12);			\
  check##TYPE ( g13s##TYPE, (double)13);			\
  check##TYPE ( g14s##TYPE, (double)14);			\
  check##TYPE ( g15s##TYPE, (double)15);			\
  check##TYPE ( g16s##TYPE, (double)16);			\
}								\
								\
void								\
test##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,			\
	    TYPE s5, TYPE s6, TYPE s7, TYPE s8,			\
	    TYPE s9, TYPE s10, TYPE s11, TYPE s12,		\
	    TYPE s13, TYPE s14, TYPE s15, TYPE s16)		\
{								\
  check##TYPE (s1, (double)1);					\
  check##TYPE (s2, (double)2);					\
  check##TYPE (s3, (double)3);					\
  check##TYPE (s4, (double)4);					\
  check##TYPE (s5, (double)5);					\
  check##TYPE (s6, (double)6);					\
  check##TYPE (s7, (double)7);					\
  check##TYPE (s8, (double)8);					\
  check##TYPE (s9, (double)9);					\
  check##TYPE (s10, (double)10);				\
  check##TYPE (s11, (double)11);				\
  check##TYPE (s12, (double)12);				\
  check##TYPE (s13, (double)13);				\
  check##TYPE (s14, (double)14);				\
  check##TYPE (s15, (double)15);				\
  check##TYPE (s16, (double)16);				\
}								\
								\
void								\
testva##TYPE (int n, ...)					\
{								\
  int i;							\
  va_list ap;							\
  if (test_va)							\
    {								\
      va_start (ap, n);						\
      for (i = 0; i < n; i++)					\
	{							\
	  TYPE t = va_arg (ap, TYPE);				\
	  check##TYPE (t, (double)i+1);				\
	}							\
      va_end (ap);						\
    }								\
}

TEST(Sfd)
TEST(Sfl)
TEST(Sdf)
TEST(Sdl)
TEST(Slf)
TEST(Sld)
TEST(Sfdl)
TEST(Sfld)
TEST(Sdfl)
TEST(Sdlf)
TEST(Slfd)
TEST(Sldf)
