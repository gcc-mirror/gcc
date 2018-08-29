/* { dg-do compile } */
/* { dg-options "-march=r2 -mno-cdx -mno-bmx -O2" } */
/* { dg-final { scan-assembler-times "stwio\tr., 0\\(r" 2 } } */
/* { dg-final { scan-assembler-times "stwio\tr., 528\\(r0\\)" 1 } } */

/* These functions should not spill to stack.  */
/* { dg-final { scan-assembler-not "addi\tsp, sp" } } */

/* On R2, stwio takes only a 12-bit displacement so foo1 and foo2 need
   to use register indirect addressing.  */

#define addr1 ((volatile int *) 0x43210)
#define addr2 ((volatile int *) 0x3210)
#define addr3 ((volatile int *) 0x210)

#define SET(l,r) __builtin_stwio ((l), (r))

void foo1 (int x) { SET (addr1, x); }
void foo2 (int x) { SET (addr2, x); }
void foo3 (int x) { SET (addr3, x); }
