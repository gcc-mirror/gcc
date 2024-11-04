/* Test the fp8 ACLE intrinsics family.  */
/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_acle.h>

#pragma GCC push_options
#pragma GCC target("arch=armv9.4-a+fp8")

/*
**test_write_fpmr_sysreg_asm_64:
**	msr	fpmr, x0
**	ret
*/
void
test_write_fpmr_sysreg_asm_64 (uint64_t val)
{
  register uint64_t fpmr asm ("fpmr") = val;
  asm volatile ("" ::"Umv"(fpmr));
}

/*
**test_write_fpmr_sysreg_asm_32:
**	msr	fpmr, x0
**	ret
*/
void
test_write_fpmr_sysreg_asm_32 (uint32_t val)
{
  register uint32_t fpmr asm ("fpmr") = val;
  asm volatile ("" ::"Umv"(fpmr));
}

/*
**test_write_fpmr_sysreg_asm_16:
**	msr	fpmr, x0
**	ret
*/
void
test_write_fpmr_sysreg_asm_16 (uint16_t val)
{
  register uint16_t fpmr asm ("fpmr") = val;
  asm volatile ("" ::"Umv"(fpmr));
}

/*
**test_write_fpmr_sysreg_asm_8:
**	msr	fpmr, x0
**	ret
*/
void
test_write_fpmr_sysreg_asm_8 (uint8_t val)
{
  register uint8_t fpmr asm ("fpmr") = val;
  asm volatile ("" ::"Umv"(fpmr));
}

/*
**test_read_fpmr_sysreg_asm_64:
**	mrs	x0, fpmr
**	ret
*/
uint64_t
test_read_fpmr_sysreg_asm_64 ()
{
  register uint64_t fpmr asm ("fpmr");
  asm volatile ("" : "=Umv"(fpmr) :);
  return fpmr;
}

/*
**test_read_fpmr_sysreg_asm_32:
**	mrs	x0, fpmr
**	ret
*/
uint32_t
test_read_fpmr_sysreg_asm_32 ()
{
  register uint32_t fpmr asm ("fpmr");
  asm volatile ("" : "=Umv"(fpmr) :);
  return fpmr;
}

/*
**test_read_fpmr_sysreg_asm_16:
**	mrs	x0, fpmr
**	ret
*/
uint16_t
test_read_fpmr_sysreg_asm_16 ()
{
  register uint16_t fpmr asm ("fpmr");
  asm volatile ("" : "=Umv"(fpmr) :);
  return fpmr;
}

/*
**test_read_fpmr_sysreg_asm_8:
**	mrs	x0, fpmr
**	ret
*/
uint8_t
test_read_fpmr_sysreg_asm_8 ()
{
  register uint8_t fpmr asm ("fpmr");
  asm volatile ("" : "=Umv"(fpmr) :);
  return fpmr;
}

#pragma GCC pop_options
