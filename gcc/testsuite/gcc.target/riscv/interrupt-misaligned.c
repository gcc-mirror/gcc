/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

/*  Make sure no stack offset are misaligned.
**  interrupt:
**  ...
**        sd\tt0,40\(sp\)
**        frcsr\tt0
**        sw\tt0,32\(sp\)
**        sd\tt1,24\(sp\)
**        fsd\tft0,8\(sp\)
**  ...
**        lw\tt0,32\(sp\)
**        fscsr\tt0
**        ld\tt0,40\(sp\)
**        ld\tt1,24\(sp\)
**        fld\tft0,8\(sp\)
**  ...
*/


void interrupt(void) __attribute__((interrupt));
void interrupt(void)
{
  asm volatile ("# clobber!":::"t0", "t1", "ft0");
}

/* { dg-final { check-function-bodies "**" "" } } */
