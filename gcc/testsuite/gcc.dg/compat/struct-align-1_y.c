/* { dg-options "-Wno-psabi" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include "compat-common.h"
#include "struct-align-1.h"

#define TEST(NAME)						\
extern char v1_##NAME;						\
extern double v2_##NAME;					\
extern int v3_##NAME;						\
								\
extern struct B1_##NAME b1_##NAME, ab1_##NAME[2];		\
extern struct B2_##NAME b2_##NAME, ab2_##NAME[2];		\
extern struct B3_##NAME b3_##NAME, ab3_##NAME[2];		\
								\
extern void pass1_##NAME (struct B1_##NAME);			\
extern void pass2_##NAME (struct B2_##NAME);			\
extern void pass3_##NAME (struct B3_##NAME);			\
extern struct B1_##NAME return1_##NAME (void);			\
extern struct B2_##NAME return2_##NAME (void);			\
extern struct B3_##NAME return3_##NAME (void);			\
								\
void								\
checkp1_##NAME (struct B1_##NAME *p)				\
{								\
  if (p->c != v1_##NAME)					\
    DEBUG_CHECK;						\
  if (p->d != v2_##NAME)					\
    DEBUG_CHECK;						\
}								\
								\
void								\
checkp2_##NAME (struct B2_##NAME *p)				\
{								\
  if (p->c != v1_##NAME)					\
    DEBUG_CHECK;						\
  if (p->a2.d != v2_##NAME)					\
    DEBUG_CHECK;						\
}								\
								\
void								\
checkp3_##NAME (struct B3_##NAME *p)				\
{								\
  if (p->c != v1_##NAME)					\
    DEBUG_CHECK;						\
  if (p->a3.d != v2_##NAME)					\
    DEBUG_CHECK;						\
  if (p->a3.i != v3_##NAME)					\
    DEBUG_CHECK;						\
}								\
								\
void								\
test_##NAME (void)						\
{								\
  struct B1_##NAME s1;						\
  struct B2_##NAME s2;						\
  struct B3_##NAME s3;						\
  DEBUG_FPUTS (DESC_##NAME);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  global variable");				\
  checkp1_##NAME (&b1_##NAME);					\
  checkp2_##NAME (&b2_##NAME);					\
  checkp3_##NAME (&b3_##NAME);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  global array");				\
  checkp1_##NAME (&ab1_##NAME[1]);				\
  checkp2_##NAME (&ab2_##NAME[1]);				\
  checkp3_##NAME (&ab3_##NAME[1]);				\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  argument");					\
  pass1_##NAME (b1_##NAME);					\
  pass2_##NAME (b2_##NAME);					\
  pass3_##NAME (b3_##NAME);					\
  DEBUG_NL;							\
  DEBUG_FPUTS ("  function result");				\
  s1 = return1_##NAME ();					\
  checkp1_##NAME (&s1);						\
  s2 = return2_##NAME ();					\
  checkp2_##NAME (&s2);						\
  s3 = return3_##NAME ();					\
  checkp3_##NAME (&s3);						\
  DEBUG_NL;							\
}

TEST (orig)
#ifndef SKIP_ATTRIBUTE
TEST (p_all)
TEST (p_inner)
TEST (p_outer)
TEST (a_max)
TEST (m_outer_p_inner)
TEST (m_inner_p_outer)
#endif
