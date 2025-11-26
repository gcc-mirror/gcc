/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-final { check-function-bodies "**" "" "" } } */

extern void abort (void);

/*
** ashrl_fn:
**...
**	asrl	r[0-9]+, r[0-9]+, #31
**...
*/
__attribute__ ((noipa))
long long ashrl_fn (long long a)
{
  long long c;

  c = a >> 31;
  c += a;
  return c;
}

/*
** ashll_fn:
**...
**	add	(r[0-9]+), \1, r[0-9]+, lsl #2
**...
*/
__attribute__ ((noipa))
long long ashll_fn (long long a)
{
  long long c;

  /* Use 34, since 33 causes PR122871.  */
  c = a << 34;
  c += a;
  return c;
}

/*
** ashll_fn2:
**...
**	lsll	r[0-9]+, r[0-9]+, #7
**...
*/
__attribute__ ((noipa))
long long ashll_fn2 (long long a /* unused */, long long x)
{
  return x << 7;
}

/*
** ashll_fn3:
**...
**	lsls	r[0-9]+, (r[0-9]+), #2
**	movs	\1, #0
**...
*/
__attribute__ ((noipa))
long long ashll_fn3 (long long x)
{
  return x << 34;
}

int main(void)
{
  long long var1 = 1;
  long long var2 = ashll_fn (var1);
  if (var2 != 0x400000001)
    abort ();

  var2 = ashrl_fn (var2);
  if (var2 != 0x400000009)
    abort ();

  var2 = ashll_fn2 (var2, 0xa987654350000002LL);
  if (var2 != 0xc3b2a1a800000100LL)
    abort ();

  return 0;
}
