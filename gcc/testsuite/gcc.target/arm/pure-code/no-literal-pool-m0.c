/* { dg-do compile } */
/* { dg-skip-if "skip override" { *-*-* } { "-mfloat-abi=hard" } { "" } } */
/* { dg-options "-mpure-code -mcpu=cortex-m0 -march=armv6s-m -mthumb -mfloat-abi=soft" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Does not use thumb1_gen_const_int.
** test_0:
**	...
**	movs	r[0-3], #0
**	...
*/
int
test_0 ()
{
  return 0;
}

/* Does not use thumb1_gen_const_int.
** test_128:
**	...
**	movs	r[0-3], #128
**	...
*/
int
test_128 ()
{
  return 128;
}

/* Does not use thumb1_gen_const_int.
** test_264:
**	...
**	movs	r[0-3], #132
**	lsls	r[0-3], r[0-3], #1
**	...
*/
int
test_264 ()
{
  return 264;
}

/* Does not use thumb1_gen_const_int.
** test_510:
**	...
**	movs	r[0-3], #255
**	lsls	r[0-3], r[0-3], #1
**	...
*/
int
test_510 ()
{
  return 510;
}

/* Does not use thumb1_gen_const_int.
** test_512:
**	...
**	movs	r[0-3], #128
**	lsls	r[0-3], r[0-3], #2
**	...
*/
int
test_512 ()
{
  return 512;
}

/* Does not use thumb1_gen_const_int.
** test_764:
**	...
**	movs	r[0-3], #191
**	lsls	r[0-3], r[0-3], #2
**	...
*/
int
test_764 ()
{
  return 764;
}

/* Does not use thumb1_gen_const_int.
** test_65536:
**	...
**	movs	r[0-3], #128
**	lsls	r[0-3], r[0-3], #9
**	...
*/
int
test_65536 ()
{
  return 65536;
}

/*
** test_0x123456:
**	...
**	movs	r[0-3], #18
**	lsls	r[0-3], r[0-3], #8
**	adds	r[0-3], r[0-3], #52
**	lsls	r[0-3], r[0-3], #8
**	adds	r[0-3], r[0-3], #86
**	...
*/
int
test_0x123456 ()
{
  return 0x123456;
}

/*
** test_0x1123456:
**	...
**	movs	r[0-3], #137
**	lsls	r[0-3], r[0-3], #8
**	adds	r[0-3], r[0-3], #26
**	lsls	r[0-3], r[0-3], #8
**	adds	r[0-3], r[0-3], #43
**	lsls	r[0-3], r[0-3], #1
**	...
*/
int
test_0x1123456 ()
{
  return 0x1123456;
}

/* With -Os, we generate:
   movs r0, #16
   lsls r0, r0, r0
   With the other optimization levels, we generate:
   movs r0, #16
   lsls r0, r0, #16
   hence the two alternatives.  */
/*
** test_0x1000010:
**	...
**	movs	r[0-3], #16
**	lsls	r[0-3], r[0-3], (#16|r[0-3])
**	adds	r[0-3], r[0-3], #1
**	lsls	r[0-3], r[0-3], #4
**	...
*/
int
test_0x1000010 ()
{
  return 0x1000010;
}

/*
** test_0x1000011:
**	...
**	movs	r[0-3], #1
**	lsls	r[0-3], r[0-3], #24
**	adds	r[0-3], r[0-3], #17
**	...
*/
int
test_0x1000011 ()
{
  return 0x1000011;
}

/*
** test_m8192:
**	...
**	movs	r[0-3], #1
**	lsls	r[0-3], r[0-3], #13
**	rsbs	r[0-3], r[0-3], #0
**	...
*/
int
test_m8192 ()
{
  return -8192;
}
