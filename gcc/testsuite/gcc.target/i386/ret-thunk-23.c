/* PR target/r84530 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mfunction-return=thunk-extern" } */

struct s { _Complex unsigned short x; };
struct s gs = { 100 + 200i };
struct s __attribute__((noinline)) foo (void) { return gs; }

/* { dg-final { scan-assembler-times "popl\[\\t \]*%ecx" 1 { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "lea\[l\]?\[\\t \]*4\\(%esp\\), %esp" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_return_thunk_ecx" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler {call[ \t]*___x86.get_pc_thunk.cx} { target { *-*-darwin* } } } } */
/* { dg-final { scan-assembler {jmp[ \t]*___x86_return_thunk} { target { *-*-darwin* } } } } */
/* { dg-final { scan-assembler-not {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler-not {call[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler-not {\tpause} } } */
/* { dg-final { scan-assembler-not {\tlfence} } } */
