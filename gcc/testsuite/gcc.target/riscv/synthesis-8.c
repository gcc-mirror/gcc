/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc_zba_zbb_zbs" } */

/* Rather than test for a specific synthesis of all these constants or
   having thousands of tests each testing one variant, we just test the
   total number of instructions.

   This isn't expected to change much and any change is worthy of a look.  */
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|ret|sh1add|sh2add|sh3add|slli|srli|xori)" 72 } } */

unsigned long foo_0xc0000000000077ff(void) { return 0xc0000000000077ffUL; }
unsigned long foo_0xc00000000000b7ff(void) { return 0xc00000000000b7ffUL; }
unsigned long foo_0xc0000000000137ff(void) { return 0xc0000000000137ffUL; }
unsigned long foo_0xc0000000000237ff(void) { return 0xc0000000000237ffUL; }
unsigned long foo_0xc0000000000437ff(void) { return 0xc0000000000437ffUL; }
unsigned long foo_0xc0000000000837ff(void) { return 0xc0000000000837ffUL; }
unsigned long foo_0xc0000000001037ff(void) { return 0xc0000000001037ffUL; }
unsigned long foo_0xc0000000002037ff(void) { return 0xc0000000002037ffUL; }
unsigned long foo_0xc0000000004037ff(void) { return 0xc0000000004037ffUL; }
unsigned long foo_0xc0000000008037ff(void) { return 0xc0000000008037ffUL; }
unsigned long foo_0xc0000000010037ff(void) { return 0xc0000000010037ffUL; }
unsigned long foo_0xc0000000020037ff(void) { return 0xc0000000020037ffUL; }
unsigned long foo_0xc0000000040037ff(void) { return 0xc0000000040037ffUL; }
unsigned long foo_0xc0000000080037ff(void) { return 0xc0000000080037ffUL; }
unsigned long foo_0xc0000000100037ff(void) { return 0xc0000000100037ffUL; }
unsigned long foo_0xe0000000000037ff(void) { return 0xe0000000000037ffUL; }
unsigned long foo_0xc00000000000d7ff(void) { return 0xc00000000000d7ffUL; }
unsigned long foo_0xc0000000000157ff(void) { return 0xc0000000000157ffUL; }
