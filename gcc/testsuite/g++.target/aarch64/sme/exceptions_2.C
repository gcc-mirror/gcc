// { dg-options "-O -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

void n_callee();
void s_callee() __arm_streaming;
void sc_callee() __arm_streaming_compatible;

void n_callee_ne() noexcept;
void s_callee_ne() noexcept __arm_streaming;
void sc_callee_ne() noexcept __arm_streaming_compatible;

void n_caller1()
{
  try
    {
      n_callee();
      sc_callee();
    }
  catch (...)
    {
      n_callee_ne();
      sc_callee_ne();
    }
}
// { dg-final { scan-assembler {_Z9n_caller1v:(?:(?!smstart|smstop).)*\tret} } }

/*
** _Z9n_caller2v:
**	...
**	cntd	(x[0-9]+)
**	str	\1, [^\n]+
**	...
**	bl	__cxa_begin_catch
**	smstart	sm
**	bl	_Z11s_callee_nev
**	smstop	sm
**	bl	__cxa_end_catch
**	...
*/
void n_caller2()
{
  try
    {
      n_callee();
      sc_callee();
    }
  catch (...)
    {
      s_callee_ne();
    }
}

/*
** _Z9s_caller1v:
**	...
**	bl	__cxa_end_catch
**	smstart	sm
**	...
*/
int s_caller1() __arm_streaming
{
  try
    {
      s_callee();
      return 1;
    }
  catch (...)
    {
      return 2;
    }
}

/*
** _Z9s_caller2v:
**	...
**	bl	__cxa_begin_catch
**	smstart	sm
**	bl	_Z11s_callee_nev
**	smstop	sm
**	bl	__cxa_end_catch
**	smstart	sm
**	...
*/
int s_caller2() __arm_streaming
{
  try
    {
      n_callee();
      return 1;
    }
  catch (...)
    {
      s_callee_ne();
      return 2;
    }
}

/*
** _Z10sc_caller1v:
**	...
**	cntd	(x[0-9]+)
**	str	\1, [^\n]+
**	mrs	(x[0-9]+), svcr
**	str	\2, ([^\n]+)
**	...
**	bl	__cxa_end_catch
**	ldr	(x[0-9]+), \3
**	tbz	\4, 0, [^\n]+
**	smstart	sm
**	...
*/
int sc_caller1() __arm_streaming_compatible
{
  try
    {
      sc_callee();
      return 1;
    }
  catch (...)
    {
      return 2;
    }
}

/*
** _Z10ls_caller1v:
**	...
**	cntd	(x[0-9]+)
**	str	\1, [^\n]+
**	...
**	bl	__cxa_begin_catch
**	smstart	sm
**	bl	_Z12sc_callee_nev
**	smstop	sm
**	bl	__cxa_end_catch
**	...
*/
__arm_locally_streaming void ls_caller1()
{
  try
    {
      sc_callee();
    }
  catch (...)
    {
      sc_callee_ne();
    }
}
