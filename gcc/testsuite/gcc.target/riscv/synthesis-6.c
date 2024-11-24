/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gcb" } */

/* Rather than test for a specific synthesis of all these constants or
   having thousands of tests each testing one variant, we just test the
   total number of instructions. 

   This isn't expected to change much and any change is worthy of a look.  */
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|bclri|li|ret|sh1add|sh2add|sh3add|slli)" 156 } } */


unsigned long foo_0xfffffffbfffff7ff(void) { return 0xfffffffbfffff7ffUL; }

unsigned long foo_0xfffffff7fffff7ff(void) { return 0xfffffff7fffff7ffUL; }

unsigned long foo_0xffffffeffffff7ff(void) { return 0xffffffeffffff7ffUL; }

unsigned long foo_0xffffffdffffff7ff(void) { return 0xffffffdffffff7ffUL; }

unsigned long foo_0xffffffbffffff7ff(void) { return 0xffffffbffffff7ffUL; }

unsigned long foo_0xffffff7ffffff7ff(void) { return 0xffffff7ffffff7ffUL; }

unsigned long foo_0xfffffefffffff7ff(void) { return 0xfffffefffffff7ffUL; }

unsigned long foo_0xfffffdfffffff7ff(void) { return 0xfffffdfffffff7ffUL; }

unsigned long foo_0xfffffbfffffff7ff(void) { return 0xfffffbfffffff7ffUL; }

unsigned long foo_0xfffff7fffffff7ff(void) { return 0xfffff7fffffff7ffUL; }

unsigned long foo_0xffffeffffffff7ff(void) { return 0xffffeffffffff7ffUL; }

unsigned long foo_0xffffdffffffff7ff(void) { return 0xffffdffffffff7ffUL; }

unsigned long foo_0xffffbffffffff7ff(void) { return 0xffffbffffffff7ffUL; }

unsigned long foo_0xffff7ffffffff7ff(void) { return 0xffff7ffffffff7ffUL; }

unsigned long foo_0xfffefffffffff7ff(void) { return 0xfffefffffffff7ffUL; }

unsigned long foo_0xfffdfffffffff7ff(void) { return 0xfffdfffffffff7ffUL; }

unsigned long foo_0xfffbfffffffff7ff(void) { return 0xfffbfffffffff7ffUL; }

unsigned long foo_0xfff7fffffffff7ff(void) { return 0xfff7fffffffff7ffUL; }

unsigned long foo_0xffeffffffffff7ff(void) { return 0xffeffffffffff7ffUL; }

unsigned long foo_0xffdffffffffff7ff(void) { return 0xffdffffffffff7ffUL; }

unsigned long foo_0xffbffffffffff7ff(void) { return 0xffbffffffffff7ffUL; }

unsigned long foo_0xff7ffffffffff7ff(void) { return 0xff7ffffffffff7ffUL; }

unsigned long foo_0xfefffffffffff7ff(void) { return 0xfefffffffffff7ffUL; }

unsigned long foo_0xfdfffffffffff7ff(void) { return 0xfdfffffffffff7ffUL; }

unsigned long foo_0xfbfffffffffff7ff(void) { return 0xfbfffffffffff7ffUL; }

unsigned long foo_0xf7fffffffffff7ff(void) { return 0xf7fffffffffff7ffUL; }

unsigned long foo_0xeffffffffffff7ff(void) { return 0xeffffffffffff7ffUL; }

unsigned long foo_0xdffffffffffff7ff(void) { return 0xdffffffffffff7ffUL; }

unsigned long foo_0xbffffffffffff7ff(void) { return 0xbffffffffffff7ffUL; }

unsigned long foo_0xffffffff7fffd7ff(void) { return 0xffffffff7fffd7ffUL; }

unsigned long foo_0xffffffff7ffdf7ff(void) { return 0xffffffff7ffdf7ffUL; }

unsigned long foo_0xffffffff7fdff7ff(void) { return 0xffffffff7fdff7ffUL; }

unsigned long foo_0xffffffff7dfff7ff(void) { return 0xffffffff7dfff7ffUL; }

unsigned long foo_0xffffffff5ffff7ff(void) { return 0xffffffff5ffff7ffUL; }

unsigned long foo_0xfffff7ff7ffff7ff(void) { return 0xfffff7ff7ffff7ffUL; }

unsigned long foo_0xffffdfff7ffff7ff(void) { return 0xffffdfff7ffff7ffUL; }

unsigned long foo_0xffff7fff7ffff7ff(void) { return 0xffff7fff7ffff7ffUL; }

unsigned long foo_0xfffdffff7ffff7ff(void) { return 0xfffdffff7ffff7ffUL; }



