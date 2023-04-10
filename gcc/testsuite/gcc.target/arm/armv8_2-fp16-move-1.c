/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-additional-options "-mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
**test_load_1:
**	...
**	vld1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
__fp16
test_load_1 (__fp16* a)
{
  return *a;
}

/*
**test_load_2:
**	...
**	vld1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
__fp16
test_load_2 (__fp16* a, int i)
{
  return a[i];
}

/*
**test_store_1:
**	...
**	vst1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
void
test_store_1 (__fp16* a, __fp16 b)
{
  *a = b;
}

/*
**test_store_2:
**	...
**	vst1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
void
test_store_2 (__fp16* a, int i, __fp16 b)
{
  a[i] = b;
}

/*
**test_load_store_1:
**	...
**	vld1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
**	vst1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
__fp16
test_load_store_1 (__fp16* a, int i, __fp16* b)
{
  a[i] = b[i];
}

/*
**test_load_store_2:
**	...
**	vld1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
**	vst1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
*/
__fp16
test_load_store_2 (__fp16* a, int i, __fp16* b)
{
  a[i] = b[i + 2];
  return a[i];
}

__fp16
test_select_1 (int sel, __fp16 a, __fp16 b)
{
  if (sel)
    return a;
  else
    return b;
}

__fp16
test_select_2 (int sel, __fp16 a, __fp16 b)
{
  return sel ? a : b;
}

__fp16
test_select_3 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a == b) ? b : c;
}

__fp16
test_select_4 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a != b) ? b : c;
}

__fp16
test_select_5 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a < b) ? b : c;
}

__fp16
test_select_6 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a <= b) ? b : c;
}

__fp16
test_select_7 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a > b) ? b : c;
}

__fp16
test_select_8 (__fp16 a, __fp16 b, __fp16 c)
{
  return (a >= b) ? b : c;
}

/* { dg-final { scan-assembler-times {vseleq\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vselgt\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vselge\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

/* { dg-final { scan-assembler-not {vmov\.f16} } }  */

int
test_compare_1 (__fp16 a, __fp16 b)
{
  if (a == b)
    return -1;
  else
    return 0;
}

int
test_compare_ (__fp16 a, __fp16 b)
{
  if (a != b)
    return -1;
  else
    return 0;
}

int
test_compare_2 (__fp16 a, __fp16 b)
{
  if (a > b)
    return -1;
  else
    return 0;
}

int
test_compare_3 (__fp16 a, __fp16 b)
{
  if (a >= b)
    return -1;
  else
    return 0;
}

int
test_compare_4 (__fp16 a, __fp16 b)
{
  if (a < b)
    return -1;
  else
    return 0;
}

int
test_compare_5 (__fp16 a, __fp16 b)
{
  if (a <= b)
    return -1;
  else
    return 0;
}

/* { dg-final { scan-assembler-not {vcmp\.f16} } }  */
/* { dg-final { scan-assembler-not {vcmpe\.f16} } }  */

/* { dg-final { scan-assembler-times {vcmpe?\.f32} 12 } }  */
