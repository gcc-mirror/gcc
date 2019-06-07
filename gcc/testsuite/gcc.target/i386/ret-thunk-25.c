/* PR target/r84530 */
/* { dg-do compile { target ia32 } } */
/* { dg-skip-if "ABI differs for return complex value" { *-*-darwin* } } */
/* { dg-options "-O2 -mfunction-return=thunk -fcheck-pointer-bounds -mmpx -fno-pic" } */

struct s { _Complex unsigned short x; };
struct s gs = { 100 + 200i };
struct s __attribute__((noinline)) foo (void) { return gs; }

/* { dg-final { scan-assembler-times "popl\[\\t \]*%ecx" 1 } } */
/* { dg-final { scan-assembler "lea\[l\]?\[\\t \]*4\\(%esp\\), %esp" } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*__x86_return_thunk_bnd_ecx" } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "call\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
