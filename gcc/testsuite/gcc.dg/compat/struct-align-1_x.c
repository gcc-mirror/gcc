#include "compat-common.h"
#include "struct-align-1.h"

#define SETUP(NAME,V1,V2,V3)				\
char v1_##NAME = V1;					\
double v2_##NAME = V2;					\
int v3_##NAME = V3;					\
							\
struct B1_##NAME b1_##NAME = { V1, V2 };		\
struct B2_##NAME b2_##NAME = { V1, { V2 } };		\
struct B3_##NAME b3_##NAME = { V1, { V2, V3 } };	\
							\
struct B1_##NAME ab1_##NAME[2] =			\
  { { V1, V2 }, { V1, V2 } };				\
struct B2_##NAME ab2_##NAME[2] =			\
  { { V1, { V2 } }, { V1, { V2 } } };			\
struct B3_##NAME ab3_##NAME[2] =			\
  { { V1, { V2, V3 } }, { V1, { V2, V3 } } };		\
							\
extern void test_##NAME (void);				\
extern void checkp1_##NAME (struct B1_##NAME *);	\
extern void checkp2_##NAME (struct B2_##NAME *);	\
extern void checkp3_##NAME (struct B3_##NAME *);	\
extern void checkg1_##NAME (void);			\
extern void checkg2_##NAME (void);			\
extern void checkg3_##NAME (void);			\
							\
void							\
pass1_##NAME (struct B1_##NAME s)			\
{							\
  checkp1_##NAME (&s);					\
}							\
							\
void							\
pass2_##NAME (struct B2_##NAME s)			\
{							\
  checkp2_##NAME (&s);					\
}							\
							\
void							\
pass3_##NAME (struct B3_##NAME s)			\
{							\
  checkp3_##NAME (&s);					\
}							\
							\
struct B1_##NAME					\
return1_##NAME (void)					\
{							\
  return ab1_##NAME[0];					\
}							\
							\
struct B2_##NAME					\
return2_##NAME (void)					\
{							\
  return ab2_##NAME[0];					\
}							\
							\
struct B3_##NAME					\
return3_##NAME (void)					\
{							\
  return ab3_##NAME[0];					\
}

#define CHECK(NAME) test_##NAME()

SETUP (orig, 49, 1.0, 111111)
#ifndef SKIP_ATTRIBUTE
SETUP (p_all, 50, 2.0, 222222)
SETUP (p_inner, 51, 3.0, 333333)
SETUP (p_outer, 52, 4.0, 444444)
SETUP (a_max, 53, 5.0, 555555)
SETUP (m_outer_p_inner, 54, 6.0, 666666)
SETUP (m_inner_p_outer, 55, 7.0, 777777) 
#endif

void
struct_align_1_x (void)
{
  DEBUG_INIT

  CHECK (orig);
#ifndef SKIP_ATTRIBUTE
  CHECK (p_all);
  CHECK (p_inner);
  CHECK (p_outer);
  CHECK (a_max);
  CHECK (m_outer_p_inner);
  CHECK (m_inner_p_outer);
#endif

  DEBUG_FINI

  if (fails != 0)
    abort ();
}
