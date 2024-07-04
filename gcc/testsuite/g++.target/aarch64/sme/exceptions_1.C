// { dg-options "-O -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

void callee_inout() __arm_inout("za");
void callee_in() noexcept __arm_in("za");
void callee_out() noexcept __arm_out("za");
void callee_normal();

/*
** _Z5test1v:
**	...
**	bl	__arm_tpidr2_save
**	...
**	bl	__cxa_begin_catch
**	bl	__cxa_end_catch
**	mov	w0, #?2
**	...
*/
__arm_new("za") int
test1 ()
{
  try
    {
      callee_inout();
      return 1;
    }
  catch (...)
    {
      return 2;
    }
}

/*
** _Z5test2v:
**	...
**	bl	__arm_tpidr2_save
**	...
**	bl	__cxa_begin_catch
**	smstart	za
**	bl	_Z10callee_outv
**	bl	_Z9callee_inv
**	smstop	za
**	bl	__cxa_end_catch
**	mov	w0, #?2
**	...
*/
__arm_new("za") int
test2 ()
{
  try
    {
      callee_inout();
      return 1;
    }
  catch (...)
    {
      callee_out();
      callee_in();
      return 2;
    }
}

/*
** _Z5test3v:
**	...
**	bl	__arm_tpidr2_save
**	...
**	smstop	za
**	...
**	bl	_Z13callee_normalv
**	...
**	bl	__cxa_begin_catch
**	smstart	za
**	bl	_Z10callee_outv
**	bl	_Z9callee_inv
**	smstop	za
**	bl	__cxa_end_catch
**	mov	w0, #?2
**	...
*/
__arm_new("za") int
test3 ()
{
  try
    {
      callee_normal();
      return 1;
    }
  catch (...)
    {
      callee_out();
      callee_in();
      return 2;
    }
}

__arm_new("za") int
test4 ()
{
  try
    {
      // No lazy save set up because this is a shared-ZA function.
      callee_inout();
      return 1;
    }
  catch (...)
    {
      callee_inout();
      return 2;
    }
}
// { dg-final { scan-assembler {_Z5test4v:(?:(?!msr\ttpidr2_el0, x[0-9]+).)*\tret} } }

/*
** _Z5test5v:
**	...
**	bl	__arm_tpidr2_save
**	...
**	smstart	za
**	...
**	bl	_Z12callee_inoutv
**	add	(x[0-9]+), [^\n]+
**	msr	tpidr2_el0, \1
**	bl	_Z13callee_normalv
**	msr	tpidr2_el0, xzr
**	smstop	za
**	...
**	bl	__cxa_begin_catch
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	msr	tpidr2_el0, xzr
**	bl	_Z12callee_inoutv
**	smstop	za
**	bl	__cxa_end_catch
**	mov	w0, #?2
**	...
*/
__arm_new("za") int
test5 ()
{
  try
    {
      callee_inout();
      callee_normal();
      return 1;
    }
  catch (...)
    {
      callee_inout();
      return 2;
    }
}

/*
** _Z5test6v:
**	...
**	msr	tpidr2_el0, x[0-9]+
**	bl	_Z13callee_normalv
**	msr	tpidr2_el0, xzr
**	...
**	bl	__cxa_begin_catch
**	bl	__cxa_end_catch
**	...
**	mrs	x[0-9]+, tpidr2_el0
**	...
**	smstart	za
**	...
**	bl	__arm_tpidr2_restore
**	msr	tpidr2_el0, xzr
**	...
*/
int
test6 () __arm_inout("za")
{
  try
    {
      callee_normal();
      callee_out();
      return 1;
    }
  catch (...)
    {
      return 2;
    }
}
