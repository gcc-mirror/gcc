// { dg-options "-O -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

#pragma GCC target "+sme2"

void private_zt0();
void out_zt0() __arm_out("zt0");
void in_zt0() __arm_in("zt0");
void inout_zt0() __arm_inout("zt0");
void preserves_zt0() __arm_preserves("zt0");

/*
** test1:
**	ret
*/
__arm_new("zt0") void test1()
{
}

/*
** test2:
**	ldr	w0, \[x0\]
**	ret
*/
__arm_new("zt0") int test2(int *ptr)
{
  return *ptr;
}

/*
** test3:
**	stp	[^\n]+
**	mov	x29, sp
**	bl	private_zt0
** (
**	mov	w0, 0
**	ldp	[^\n]+
** |
**	ldp	[^\n]+
**	mov	w0, 0
** )
**	ret
*/
__arm_new("zt0") int test3()
{
  private_zt0();
  return 0;
}

/*
** test4:
**	...
**	mrs	x0, tpidr2_el0
**	cbz	x0, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	zero	{ zt0 }
**	smstart	za
**	bl	in_zt0
**	smstop	za
**	ldp	[^\n]+
**	ret
*/
__arm_new("zt0") void test4()
{
  in_zt0(); // Uses zeroed contents.
}

/*
** test5:
**	...
**	mrs	x0, tpidr2_el0
**	cbz	x0, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	smstop	za
**	bl	private_zt0
**	smstart	za
**	bl	out_zt0
**	bl	in_zt0
**	...
**	smstop	za
**	bl	private_zt0
**	ldp	[^\n]+
**	ret
*/
__arm_new("zt0") void test5()
{
  private_zt0();
  out_zt0();
  in_zt0();
  private_zt0();
}

// Despite the long test, there shouldn't be too much scope for variation
// here.  The point is both to test correctness and code quality.
/*
** test6:
**	stp	[^\n]+
**	mov	x29, sp
**	mrs	x0, tpidr2_el0
**	cbz	x0, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	smstart	za
**	bl	out_zt0
**	...
**	str	zt0, [^\n]+
**	smstop	za
**	bl	private_zt0
**	smstart	za
**	...
**	ldr	zt0, [^\n]+
**	bl	in_zt0
**	smstop	za
**	ldp	[^\n]+
**	ret
*/
__arm_new("zt0") void test6()
{
  out_zt0();
  private_zt0();
  in_zt0();
}

// Rely on previous tests for the part leading up to the smstart.
/*
** test7:
**	...
**	smstart	za
**	bl	out_zt0
**	bl	in_zt0
**	...
**	smstop	za
**	bl	private_zt0
**	smstart	za
**	bl	out_zt0
**	bl	in_zt0
**	smstop	za
**	ldp	[^\n]+
**	ret
*/
__arm_new("zt0") void test7()
{
  out_zt0();
  in_zt0();
  private_zt0();
  out_zt0();
  in_zt0();
}

/*
** test8:
**	...
**	smstart	za
**	bl	out_zt0
**	bl	in_zt0
**	...
**	smstop	za
**	bl	private_zt0
**	smstart	za
**	bl	out_zt0
**	bl	in_zt0
**	...
**	smstop	za
**	bl	private_zt0
**	ldp	[^\n]+
**	ret
*/
__arm_new("zt0") void test8()
{
  out_zt0();
  in_zt0();
  private_zt0();
  out_zt0();
  in_zt0();
  private_zt0();
}

/*
** test9:
**	...
**	str	zt0, [^\n]+
**	smstop	za
**	bl	private_zt0
**	bl	private_zt0
**	bl	private_zt0
**	bl	private_zt0
**	smstart	za
**	...
**	ldr	zt0, [^\n]+
**	bl	in_zt0
**	smstop	za
**	...
*/
__arm_new("zt0") void test9()
{
  out_zt0();
  private_zt0();
  private_zt0();
  private_zt0();
  private_zt0();
  in_zt0();
}

/*
** test10:
**	ldr	(w[0-9]+), \[x0\]
**	cbz	\1, [^\n]+
**	ldr	[^\n]+
**	add	[^\n]+
**	str	[^\n]+
**	ret
**	...
*/
__arm_new("zt0") void test10(volatile int *ptr)
{
  if (__builtin_expect (*ptr != 0, 1))
    *ptr = *ptr + 1;
  else
    inout_zt0();
}

/*
** test11:
**	...
**	ldr	w[0-9]+, [^\n]+
**	add	(w[0-9]+), [^\n]+
**	str	\1, [^\n]+
**	...
**	ret
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	bl	inout_zt0
**	ldr	(w[0-9]+), [^\n]+
**	cbnz	\2, [^\n]+
**	smstop	za
**	...
*/
__arm_new("zt0") void test11(volatile int *ptr)
{
  if (__builtin_expect (*ptr == 0, 0))
    do
      inout_zt0();
    while (*ptr);
  else
    *ptr += 1;
}

__arm_new("zt0") void test12(volatile int *ptr)
{
  do
    {
      inout_zt0();
      private_zt0();
    }
  while (*ptr);
  out_zt0();
  in_zt0();
}
