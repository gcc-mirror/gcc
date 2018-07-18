/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "stw\tr., 12816\\(r\[2-9\]\\)" 1 } } */
/* { dg-final { scan-assembler-times "stw\tr., 12816\\(r0\\)" 1 } } */
/* { dg-final { scan-assembler-times "stw\tr., 528\\(r0\\)" 1 } } */

/* These functions should not spill to stack.  */
/* { dg-final { scan-assembler-not "addi\tsp, sp" } } */
/* { dg-final { scan-assembler-not "spdeci" } } */

#define addr1 ((volatile int *) 0x43210)
#define addr2 ((volatile int *) 0x3210)
#define addr3 ((volatile int *) 0x210)

#define SET(l,r) (*(l) = (r))

void foo1 (int x) { SET (addr1, x); }
void foo2 (int x) { SET (addr2, x); }
void foo3 (int x) { SET (addr3, x); }
