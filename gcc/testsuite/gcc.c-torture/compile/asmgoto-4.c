/* Check that LRA really puts output reloads for p4 in two successors blocks */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* { dg-options "-O0 -fdump-rtl-reload" } */

int f (int *p1, int *p2, int *p3, int *p4) {
  asm volatile goto (
            ""
            : "=r" (*p2), "=a" (p4)
            : "r" (*p2), "r" (p2)
            : "r8", "r9" : lab, lab2);
 lab: return p2 - p4;
 lab2: return p3 - p4;
}
/* { dg-final { scan-rtl-dump-times "Inserting insn reload after in bb" 2 "reload" } } */
