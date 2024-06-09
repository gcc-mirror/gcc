/* { dg-do compile } */
/* { dg-options "-O2" } */
typedef unsigned long int mp_limb_t;
typedef const mp_limb_t * mp_srcptr;

int
refmpn_tstbit_bad (mp_srcptr ptr, unsigned long bit)
{
  return (((ptr)[(bit)/(32 - 0)] & (((mp_limb_t) 1L) << ((bit)%(32 - 0)))) != 0);
}

/* 32bit produces:
        btl     %eax, %edx
        setc    %al
        movzbl  %al, %eax
 */
/* { dg-final { scan-assembler "bt\[ql\]" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "setc" { target {  ! lp64 } } } } */

/* 64bit produces:
        shrq    %cl, %rax
        andl	$1, %eax
 */
/* { dg-final { scan-assembler-times "shr\[qx\]" 2 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times "andl" 2 { target { lp64 } } } } */
