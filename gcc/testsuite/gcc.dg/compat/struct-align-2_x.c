/* Disable this test for 16-bit targets.  */

#include <limits.h>

#if !(defined __GNUC__) || (INT_MAX > 32767)

#include "compat-common.h"
#include "struct-align-2.h"

#define SETUP(NAME,V1,V2,V3)					\
struct outer_##NAME {						\
  int i;							\
  struct epoll_event_##NAME ee;					\
};								\
								\
unsigned int v1_##NAME = V1;					\
unsigned int v2_##NAME = V2;					\
unsigned long long v3_##NAME = V3;				\
								\
struct outer_##NAME s_##NAME[2] =				\
 { {V1, { V2, V3 } }, { V1, { V2, V3 } } };			\
								\
extern void test_##NAME (void);					\
extern void checkp_##NAME (struct outer_##NAME *);		\
extern void checkg_##NAME (void);				\
								\
void								\
pass_##NAME (struct outer_##NAME s)				\
{								\
  checkp_##NAME (&s);						\
}								\
								\
struct outer_##NAME						\
return_##NAME (void)						\
{								\
  return s_##NAME[0];						\
}

#define CHECK(NAME)						\
  test_##NAME()

SETUP (orig,101, 102, 0x0101010101010101ULL)
#ifndef SKIP_ATTRIBUTE
SETUP (structmax, 103, 104, 0x1212121212121212ULL)
SETUP (struct4, 105, 106, 0x2323232323232323ULL)
SETUP (struct8, 107, 108, 0x3434343434343434ULL)
SETUP (data4, 109, 110, 0x4545454545454545ULL)
SETUP (data8, 111, 112, 0x5656565656565656ULL)
SETUP (p, 113, 114, 0x6767676767676767ULL)
SETUP (pstruct4, 115, 116, 0x7878787878787878ULL)
SETUP (pstruct8, 117, 118, 0x8989898989898989ULL)
SETUP (pdata4, 119, 120, 0x9A9A9A9A9A9A9A9AULL)
SETUP (pdata8, 121, 122, 0xABABABABABABABABULL)
#endif

void
struct_align_2_x (void)
{
  DEBUG_INIT

  CHECK (orig);
#ifndef SKIP_ATTRIBUTE
  CHECK (structmax);
  CHECK (struct4);
  CHECK (struct8);
  CHECK (data4);
  CHECK (data8);
  CHECK (p);
  CHECK (pstruct4);
  CHECK (pstruct8);
  CHECK (pdata4);
  CHECK (pdata8);
#endif

  DEBUG_FINI

  if (fails != 0)
    abort ();
}

#else

void struct_align_2_x (void) {}

#endif  /* INT_MAX */
