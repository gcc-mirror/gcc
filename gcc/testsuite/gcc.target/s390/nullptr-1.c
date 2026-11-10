/* { dg-do compile } */
/* { dg-options "-O2 -std=c23 -Wno-psabi" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* ABI tests for passing/returning values of type nullptr_t.  */

#include <stddef.h>

/*
** test_ret_callee:
**	lghi	%r2,0
**	br	%r14
*/
[[gnu::noinline]]
nullptr_t
test_ret_callee (void)
{
  __asm__ volatile ("");
  return nullptr;
}

/*
** test_ret_caller:
**	...
**	brasl	%r14,test_ret_callee@PLT
**	...
**	stg	%r2,160\(%r15\)
**	...
*/
void
test_ret_caller (void)
{
  volatile nullptr_t x = test_ret_callee ();
}

/*
** test_arg_1_call:
**	...
**	lghi	%r2,0
**	...
*/
extern void test_arg_1 (nullptr_t);
void
test_arg_1_call (void)
{
  test_arg_1 (nullptr);
}

/*
** test_arg_2_call:
**	...
**	lghi	%r3,0
**	...
*/
extern void test_arg_2 (long, nullptr_t);
void
test_arg_2_call (void)
{
  test_arg_2 (42, nullptr);
}

/*
** test_arg_3_call:
**	...
**	lghi	%r4,0
**	...
*/
extern void test_arg_3 (long, long, nullptr_t);
void
test_arg_3_call (void)
{
  test_arg_3 (42, 42, nullptr);
}

/*
** test_arg_4_call:
**	...
**	lghi	%r5,0
**	...
*/
extern void test_arg_4 (long, long, long, nullptr_t);
void
test_arg_4_call (void)
{
  test_arg_4 (42, 42, 42, nullptr);
}

/*
** test_arg_5_call:
**	...
**	lghi	%r6,0
**	...
*/
extern void test_arg_5 (long, long, long, long, nullptr_t);
void
test_arg_5_call (void)
{
  test_arg_5 (42, 42, 42, 42, nullptr);
}

/*
** test_arg_6_call:
**	...
**	(mvghi	160\(%r15\),0|stg	%r[0-9]+,160\(%r15\))
**	...
*/
extern void test_arg_6 (long, long, long, long, long, nullptr_t);
void
test_arg_6_call (void)
{
  test_arg_6 (42, 42, 42, 42, 42, nullptr);
}
