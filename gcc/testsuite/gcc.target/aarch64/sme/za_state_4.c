// { dg-options "-O -fno-optimize-sibling-calls -fno-stack-clash-protection" }
// { dg-final { check-function-bodies "**" "" } }

void private_za();
void out_za() __arm_out("za");
void in_za() __arm_in("za");
void inout_za() __arm_inout("za");
void preserves_za() __arm_preserves("za");

/*
** test1:
**	ret
*/
__arm_new("za") void test1()
{
}

/*
** test2:
**	ldr	w0, \[x0\]
**	ret
*/
__arm_new("za") int test2(int *ptr)
{
  return *ptr;
}

/*
** test3:
**	stp	[^\n]+
**	mov	x29, sp
**	bl	private_za
** (
**	mov	w0, 0
**	ldp	[^\n]+
** |
**	ldp	[^\n]+
**	mov	w0, 0
** )
**	ret
*/
__arm_new("za") int test3()
{
  private_za();
  return 0;
}

/*
** test4:
**	...
**	mrs	x0, tpidr2_el0
**	cbz	x0, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	zero	{ za }
**	smstart	za
**	bl	in_za
**	smstop	za
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test4()
{
  in_za(); // Uses zeroed contents.
}

/*
** test5:
**	...
**	mrs	x0, tpidr2_el0
**	cbz	x0, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	smstop	za
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test5()
{
  private_za();
  out_za();
  in_za();
  private_za();
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
**	bl	out_za
**	rdsvl	(x[0-9]+), #1
**	mul	(x[0-9]+), \1, \1
**	sub	sp, sp, \2
**	mov	(x[0-9]+), sp
**	stp	\3, \1, \[x29, #?16\]
**	add	(x[0-9]+), x29, #?16
**	msr	tpidr2_el0, \4
**	bl	private_za
** (
**	add	(x[0-9]+), x29, #?16
**	mrs	(x[0-9]+), tpidr2_el0
**	cbnz	\6, [^\n]+
**	smstart	za
**	mov	x0, \5
** |
**	add	x0, x29, #?16
**	mrs	(x[0-9]+), tpidr2_el0
**	cbnz	\6, [^\n]+
**	smstart	za
** )
**	bl	__arm_tpidr2_restore
**	msr	tpidr2_el0, xzr
**	bl	in_za
**	smstop	za
**	mov	sp, x29
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test6()
{
  out_za();
  private_za();
  in_za();
}

// Rely on previous tests for the part leading up to the smstart.
/*
** test7:
**	...
**	smstart	za
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	smstop	za
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test7()
{
  out_za();
  in_za();
  private_za();
  out_za();
  in_za();
}

/*
** test8:
**	...
**	smstart	za
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test8()
{
  out_za();
  in_za();
  private_za();
  out_za();
  in_za();
  private_za();
}

/*
** test9:
**	...
**	msr	tpidr2_el0, x[0-9]+
**	bl	private_za
**	bl	private_za
**	bl	private_za
**	bl	private_za
**	add	x[0-9]+, x29, #?16
**	mrs	x[0-9]+, tpidr2_el0
**	...
*/
__arm_new("za") void test9()
{
  out_za();
  private_za();
  private_za();
  private_za();
  private_za();
  in_za();
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
__arm_new("za") void test10(volatile int *ptr)
{
  if (__builtin_expect (*ptr != 0, 1))
    *ptr = *ptr + 1;
  else
    inout_za();
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
**	bl	inout_za
**	ldr	(w[0-9]+), [^\n]+
**	cbnz	\2, [^\n]+
**	smstop	za
**	...
*/
__arm_new("za") void test11(volatile int *ptr)
{
  if (__builtin_expect (*ptr == 0, 0))
    do
      inout_za();
    while (*ptr);
  else
    *ptr += 1;
}

__arm_new("za") void test12(volatile int *ptr)
{
  do
    {
      inout_za();
      private_za();
    }
  while (*ptr);
  out_za();
  in_za();
}

/*
** test13:
**	stp	[^\n]+
**	...
**	stp	[^\n]+
**	...
**	bl	__arm_tpidr2_save
**	...
**	msr	tpidr2_el0, x[0-9]+
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	bl	inout_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	cbnz	[^\n]+
**	smstart	za
**	msr	tpidr2_el0, xzr
**	bl	out_za
**	bl	in_za
**	...
**	smstop	za
**	...
*/
__arm_new("za") void test13(volatile int *ptr)
{
  do
    {
      private_za();
      inout_za();
      private_za();
    }
  while (*ptr);
  out_za();
  in_za();
}

/*
** test14:
**	...
**	bl	__arm_tpidr2_save
**	...
**	smstart	za
**	bl	inout_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	bl	out_za
**	bl	in_za
**	smstop	za
**	...
*/
__arm_new("za") void test14(volatile int *ptr)
{
  do
    inout_za();
  while (*ptr);
  out_za();
  in_za();
}

/*
** test15:
**	...
**	bl	__arm_tpidr2_save
**	...
**	smstart	za
**	bl	out_za
**	bl	in_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	smstop	za
**	bl	private_za
**	ldr	[^\n]+
**	ldp	[^\n]+
**	ret
*/
__arm_new("za") void test15(volatile int *ptr)
{
  do
    {
      out_za();
      in_za();
    }
  while (*ptr);
  private_za();
}

/*
** test16:
**	...
**	bl	__arm_tpidr2_save
**	...
**	smstart	za
**	b	[^\n]+
-- loop:
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	msr	tpidr2_el0, xzr
-- loop_entry:
**	bl	inout_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	bl	private_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	msr	tpidr2_el0, xzr
**	smstop	za
**	bl	private_za
**	...
*/
__arm_new("za") void test16(volatile int *ptr)
{
  do
    {
      inout_za();
      private_za();
    }
  while (*ptr);
  private_za();
}

/*
** test17:
**	...
**	bl	private_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	...
**	msr	tpidr2_el0, xzr
**	...
**	smstop	za
**	...
*/
__arm_new("za") void test17(volatile int *ptr)
{
  do
    {
      inout_za();
      private_za();
    }
  while (*ptr);
}

/*
** test18:
**	ldr	w[0-9]+, [^\n]+
**	cbnz	w[0-9]+, [^\n]+
**	ret
**	...
**	smstop	za
**	bl	private_za
**	...
*/
__arm_new("za") void test18(volatile int *ptr)
{
  if (__builtin_expect (*ptr, 0))
    {
      out_za();
      in_za();
      private_za();
    }
}

/*
** test19:
**	...
**	ldr	w[0-9]+, [^\n]+
**	cbz	w[0-9]+, [^\n]+
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstop	za
**	bl	private_za
**	...
*/
__arm_new("za") void test19(volatile int *ptr)
{
  if (__builtin_expect (*ptr != 0, 1))
    private_za();
  else
    do
      {
	inout_za();
	private_za();
      }
    while (*ptr);
}

/*
** test20:
**	...
**	bl	a20
**	(?:(?!x0).)*
**	bl	b20
**	...
**	mov	([wx][0-9]+), [wx]0
**	...
**	bl	__arm_tpidr2_restore
**	...
**	mov	[wx]0, \1
**	...
**	bl	c20
**	...
*/
__arm_new("za") void test20()
{
  extern int a20() __arm_inout("za");
  extern int b20(int);
  extern void c20(int) __arm_inout("za");
  c20(b20(a20()));
}

/*
** test21:
**	...
**	bl	a21
**	(?:(?!x0).)*
**	bl	b21
**	...
**	mov	(x[0-9]+), x0
**	...
**	bl	__arm_tpidr2_restore
**	...
**	mov	x0, \1
**	...
**	bl	c21
**	...
*/
__arm_new("za") void test21()
{
  extern __UINT64_TYPE__ a21() __arm_inout("za");
  extern __UINT64_TYPE__ b21(__UINT64_TYPE__);
  extern void c21(__UINT64_TYPE__) __arm_inout("za");
  c21(b21(a21()));
}

/*
** test22:
**	(?:(?!rdsvl).)*
**	rdsvl	x[0-9]+, #1
**	(?:(?!rdsvl).)*
*/
__arm_new("za") void test22(volatile int *ptr)
{
  inout_za();
  if (*ptr)
    *ptr += 1;
  else
    private_za();
  private_za();
  in_za();
}

/*
** test23:
**	(?:(?!__arm_tpidr2_save).)*
**	bl	__arm_tpidr2_save
**	(?:(?!__arm_tpidr2_save).)*
*/
__arm_new("za") void test23(volatile int *ptr)
{
  if (*ptr)
    *ptr += 1;
  else
    inout_za();
  inout_za();
}

/*
** test24:
**	...
**	bl	in_za
**	...
**	incb	x1
**	...
**	bl	out_za
**	bl	inout_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	incb	x1
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	incb	x1
**	...
**	smstop	za
**	...
**	bl	private_za
**	...
**	ret
*/
__arm_new("za") void test24()
{
  in_za();
  asm ("incb\tx1" ::: "x1", "za");
  out_za();
  inout_za();
  private_za();
  asm ("incb\tx1" ::: "x1", "za");
  private_za();
  asm ("incb\tx1" ::: "x1", "za");
  in_za();
  private_za();
}
