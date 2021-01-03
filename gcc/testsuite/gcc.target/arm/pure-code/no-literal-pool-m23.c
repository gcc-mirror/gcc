/* { dg-do compile } */
/* { dg-skip-if "skip override" { *-*-* } { "-mfloat-abi=hard" } { "" } } */
/* { dg-options "-mpure-code -mcpu=cortex-m23 -march=armv8-m.base -mthumb -mfloat-abi=soft" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** testi:
**	...
**	movs	r[0-3], #1
**	lsls	r[0-3], #13
**	rsbs	r[0-3], #0
**	...
*/
int
testi (int *p)
{
  if (*p > 0x12345678)
    return *p-8192;
  else
    return *p+8192;
}

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
**	movw	r[0-3], #264
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
**	movw	r[0-3], #510
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
**	movw	r[0-3], #512
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
**	movw	r[0-3], #764
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

/* Does not use thumb1_gen_const_int.
** test_0x123456:
**	...
**	movw	r[0-3], #13398
**	movt	r[0-3], 18
**	...
*/
int
test_0x123456 ()
{
  return 0x123456;
}

/* Does not use thumb1_gen_const_int.
** test_0x1123456:
**	...
**	movw	r[0-3], #13398
**	movt	r[0-3], 274
**	...
*/
int
test_0x1123456 ()
{
  return 0x1123456;
}

/* Does not use thumb1_gen_const_int.
** test_0x1000010:
**	...
**	movs	r[0-3], #16
**	movt	r[0-3], 256
**	...
*/
int
test_0x1000010 ()
{
  return 0x1000010;
}

/* Does not use thumb1_gen_const_int.
** test_0x1000011:
**	...
**	movs	r[0-3], #17
**	movt	r[0-3], 256
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
**	lsls	r[0-3], #13
**	rsbs	r[0-3], #0
**	...
*/
int
test_m8192 ()
{
  return -8192;
}
