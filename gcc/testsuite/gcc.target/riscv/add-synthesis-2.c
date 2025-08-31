/* { dg-do compile { target rv64 } } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */



#define T(C)  int foo_##C (int x) { return x + C; }
#define TM(C)  int foo_M##C (int x) { return x + -C; }

/* These cases were selected because they all can be synthesized
   at expansion time without synthesizing the constant directly.

   That makes the assembler scan testing simpler.  I've verified
   by hand that cases that should synthesize the constant do in
   fact still generate code that way.  */
T (2050)
T (4094)

TM (2049)
TM (4096)

/* We have 4/5 tests which should use shNadd insns and 4
   which used paired addi insns.  */
/* { dg-final { scan-assembler-times "addiw\t" 8 } } */
