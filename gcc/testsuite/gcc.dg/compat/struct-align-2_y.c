/* Disable this test for 16-bit targets.  */

#include <limits.h>

#if !(defined __GNUC__) || (INT_MAX > 32767)

#include "compat-common.h"
#include "struct-align-2.h"

#define TEST(NAME)						\
struct outer_##NAME {						\
  int i;							\
  struct epoll_event_##NAME ee;					\
};								\
								\
extern unsigned int v1_##NAME;					\
extern unsigned int v2_##NAME;					\
extern unsigned long long v3_##NAME;				\
								\
extern struct outer_##NAME s_##NAME[2];				\
								\
extern void pass_##NAME (struct outer_##NAME);			\
extern struct outer_##NAME return_##NAME (void);		\
								\
void								\
checkp_##NAME (struct outer_##NAME *p)				\
{								\
  if (p->i != v1_##NAME)					\
    DEBUG_CHECK;						\
  if (p->ee.events != v2_##NAME)				\
    DEBUG_CHECK;						\
  if (p->ee.data != v3_##NAME)					\
    DEBUG_CHECK;						\
}								\
								\
void								\
test_##NAME (void)						\
{								\
  struct outer_##NAME s;					\
  DEBUG_FPUTS (DESC_##NAME);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  global array");				\
  checkp_##NAME (&s_##NAME[0]);					\
  checkp_##NAME (&s_##NAME[1]);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  argument");					\
  pass_##NAME (s_##NAME[0]);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  function result");				\
  s = return_##NAME ();						\
  checkp_##NAME (&s);						\
  DEBUG_NL;							\
}

TEST (orig)
#ifndef SKIP_ATTRIBUTE
TEST (structmax)
TEST (struct4)
TEST (struct8)
TEST (data4)
TEST (data8)
TEST (p)
TEST (pstruct4)
TEST (pstruct8)
TEST (pdata4)
TEST (pdata8)
#endif

#else

int i;  /* prevent compiling an empty file */

#endif  /* INT_MAX */
