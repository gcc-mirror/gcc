/* Make sure that short memcpy's with constant length are emitted
   without loop statements.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

/* 3 MVCs */
void
*memcpy1(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 700);
}

/* NOP */
void
*memcpy2(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 0);
}

/* 1 MVC */
void
*memcpy3(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 256);
}

/* 2 MVCs */
void
*memcpy4(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 512);
}

/* 3 MVCs */
void
*memcpy5(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 768);
}

/* Loop with 2 MVCs */
void
*memcpy6(void *dest, const void *src)
{
  return __builtin_memcpy (dest, src, 1537);
}

/* memcpy6 uses a loop - check for the two load address instructions
   used to increment src and dest.  */
/* { dg-final { scan-assembler-times "la" 2 } } */

/* { dg-final { scan-assembler-times "mvc" 11 } } */
