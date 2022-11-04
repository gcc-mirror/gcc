/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* ARMv8-M expectation with target { ! arm_cmse_clear_ok }.  */
/* ARMv8.1-M expectation with target arm_cmse_clear_ok.  */
/* { dg-final { check-function-bodies "**" "" "" } } */

int __attribute__ ((cmse_nonsecure_call)) (*ns_foo) (void);
int (*s_bar) (void);
int __attribute__ ((cmse_nonsecure_call)) (**ns_foo2) (void);
int (**s_bar2) (void);

typedef int __attribute__ ((cmse_nonsecure_call)) ns_foo_t (void);
typedef int s_bar_t (void);
typedef int __attribute__ ((cmse_nonsecure_call)) (* ns_foo_ptr) (void);
typedef int (*s_bar_ptr) (void);

/*
** nonsecure0:  { target arm_cmse_clear_ok }
**	...
**	blxns	r[0-3]
**	...
*/
/*
** nonsecure0: { target { ! arm_cmse_clear_ok } }
**	...
**	bl	__gnu_cmse_nonsecure_call
**	...
*/
int nonsecure0 (ns_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
** nonsecure1:  { target arm_cmse_clear_ok }
**	...
**	blxns	r[0-3]
**	...
*/
/*
** nonsecure1: { target { ! arm_cmse_clear_ok } }
**	...
**	bl	__gnu_cmse_nonsecure_call
**	...
*/
int nonsecure1 (ns_foo_t ** ns_foo_p)
{
  return (*ns_foo_p) ();
}

/*
** nonsecure2:  { target arm_cmse_clear_ok }
**	...
** (
**	blxns	r[0-3]
** |
**	b	nonsecure0
** )
**	...
*/
/*
** nonsecure2: { target { ! arm_cmse_clear_ok } }
**	...
** (
**	bl	__gnu_cmse_nonsecure_call
** |
**	b	nonsecure0
** )
**	...
*/
int nonsecure2 (ns_foo_ptr ns_foo_p)
{
  return ns_foo_p ();
}

/*
** nonsecure3:  { target arm_cmse_clear_ok }
**	...
**	blxns	r[0-3]
**	...
*/
/*
** nonsecure3: { target { ! arm_cmse_clear_ok } }
**	...
**	bl	__gnu_cmse_nonsecure_call
**	...
*/
int nonsecure3 (ns_foo_ptr * ns_foo_p)
{
  return (*ns_foo_p) ();
}

/*
** secure0:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** )
**	...
*/
int secure0 (s_bar_t * s_bar_p)
{
  return s_bar_p ();
}

/*
** secure1:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** )
**	...
*/
int secure1 (s_bar_t ** s_bar_p)
{
  return (*s_bar_p) ();
}

/*
** secure2:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** |
**	b	secure0
** )
**	...
*/
int secure2 (s_bar_ptr s_bar_p)
{
  return s_bar_p ();
}

/*
** secure3:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** |
**	b	secure1
** )
**	...
*/
int secure3 (s_bar_ptr * s_bar_p)
{
  return (*s_bar_p) ();
}

/*
** nonsecure4:  { target arm_cmse_clear_ok }
**	...
**	blxns	r[0-3]
**	...
*/
/*
** nonsecure4: { target { ! arm_cmse_clear_ok } }
**	...
**	bl	__gnu_cmse_nonsecure_call
**	...
*/
int nonsecure4 (void)
{
  return ns_foo ();
}

/*
** nonsecure5:  { target arm_cmse_clear_ok }
**	...
**	blxns	r[0-3]
**	...
*/
/*
** nonsecure5: { target { ! arm_cmse_clear_ok } }
**	...
**	bl	__gnu_cmse_nonsecure_call
**	...
*/
int nonsecure5 (void)
{
  return (*ns_foo2) ();
}

/*
** secure4:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** )
**	...
*/
int secure4 (void)
{
  return s_bar ();
}

/*
** secure5:
**	...
** (
**	bx	r[0-3]
** |
**	blx	r[0-3]
** )
**	...
*/
int secure5 (void)
{
  return (*s_bar2) ();
}
