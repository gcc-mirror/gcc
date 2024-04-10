// { dg-options "-O2 -fno-optimize-sibling-calls -fno-stack-clash-protection" }
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
void test1() __arm_inout("za")
{
}

/*
** test2:
**	ldr	w0, \[x0\]
**	ret
*/
int test2(int *ptr) __arm_inout("za")
{
  return *ptr;
}

/*
** test3:
**	...
**	sub	sp, sp, x[0-9]+
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
int test3() __arm_inout("za")
{
  private_za();
  return 0;
}

/*
** test4:
**	stp	[^\n]+
**	[^\n]+
**	bl	in_za
**	ldp	[^\n]+
**	ret
*/
void test4() __arm_inout("za")
{
  in_za();
}

/*
** test5:
**	...
**	smstop	za
**	...
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	...
**	sub	sp, sp, x[0-9]+
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
void test5() __arm_inout("za")
{
  private_za();
  out_za();
  in_za();
  private_za();
}

/*
** test6:
**	...
**	bl	out_za
**	...
**	sub	sp, sp, x[0-9]+
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
**	bl	in_za
**	...
*/
void test6() __arm_inout("za")
{
  out_za();
  private_za();
  in_za();
}

/*
** test7:
**	stp	[^\n]+
**	[^\n]+
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	ldp	[^\n]+
**	ret
*/
void test7() __arm_inout("za")
{
  out_za();
  in_za();
  private_za();
  out_za();
  in_za();
}

/*
** test8:
**	stp	[^\n]+
**	[^\n]+
**	bl	out_za
**	bl	in_za
**	smstop	za
**	bl	private_za
**	smstart	za
**	bl	out_za
**	bl	in_za
**	...
**	sub	sp, sp, x[0-9]+
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
**	ret
*/
void test8() __arm_inout("za")
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
**	stp	[^\n]+
**	[^\n]+
**	bl	out_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	bl	private_za
**	bl	private_za
**	bl	private_za
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
void test9() __arm_inout("za")
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
void test10(volatile int *ptr) __arm_inout("za")
{
  if (__builtin_expect (*ptr != 0, 1))
    *ptr = *ptr + 1;
  else
    inout_za();
}

/*
** test11:
**	(?!.*(\t__arm|\tza|tpidr2_el0)).*
*/
void test11(volatile int *ptr) __arm_inout("za")
{
  if (__builtin_expect (*ptr == 0, 0))
    do
      inout_za();
    while (*ptr);
  else
    *ptr += 1;
}

void test12(volatile int *ptr) __arm_inout("za")
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
-- loop:
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	bl	inout_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	smstart	za
**	msr	tpidr2_el0, xzr
**	bl	out_za
**	bl	in_za
**	[^\n]+
**	[^\n]+
**	ldp	[^\n]+
**	ret
*/
void test13(volatile int *ptr) __arm_inout("za")
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
**	bl	inout_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	bl	out_za
**	bl	in_za
**	...
*/
void test14(volatile int *ptr) __arm_inout("za")
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
**	bl	out_za
**	bl	in_za
**	ldr	[^\n]+
**	cbnz	[^\n]+
**	...
**	stp	[^\n]+
**	...
**	msr	tpidr2_el0, [^\n]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
void test15(volatile int *ptr) __arm_inout("za")
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
**	stp	[^\n]+
**	...
**	stp	[^\n]+
**	...
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
**	...
**	bl	private_za
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
void test16(volatile int *ptr) __arm_inout("za")
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
-- loop:
**	bl	inout_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
**	cbnz	[^\n]+
**	[^\n]+
**	[^\n]+
**	ldp	[^\n]+
**	ret
*/
void test17(volatile int *ptr) __arm_inout("za")
{
  do
    {
      inout_za();
      private_za();
      while (*ptr)
	ptr += 1;
    }
  while (*ptr);
}

/*
** test18:
**	ldr	w[0-9]+, [^\n]+
**	cbnz	w[0-9]+, [^\n]+
**	ret
**	...
**	bl	out_za
**	bl	in_za
**	...
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	bl	__arm_tpidr2_restore
**	...
**	msr	tpidr2_el0, xzr
**	...
*/
void test18(volatile int *ptr) __arm_inout("za")
{
  if (__builtin_expect (*ptr, 0))
    {
      out_za();
      in_za();
      private_za();
    }
}

void test19(volatile int *ptr) __arm_inout("za")
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
void test20() __arm_inout("za")
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
void test21() __arm_inout("za")
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
void test22(volatile int *ptr) __arm_inout("za")
{
  inout_za();
  if (*ptr)
    *ptr += 1;
  else
    private_za();
  private_za();
  in_za();
}

void test23(volatile int *ptr) __arm_inout("za")
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
**	msr	tpidr2_el0, x[0-9]+
**	...
**	bl	private_za
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	ret
*/
void test24() __arm_inout("za")
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
