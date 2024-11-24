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
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|ret|sh1add|sh2add|sh3add|slli)" 248 } } */

unsigned long foo_0x1200000120000000 (void) { return 0x1200000120000000UL; }
unsigned long foo_0x120000402000000 (void) { return 0x120000402000000UL; }
unsigned long foo_0x1200100020000 (void) { return 0x1200100020000UL; }
unsigned long foo_0x14000000a0000000 (void) { return 0x14000000a0000000UL; }
unsigned long foo_0x140000082000000 (void) { return 0x140000082000000UL; }
unsigned long foo_0x14000080200000 (void) { return 0x14000080200000UL; }
unsigned long foo_0x1400080020000 (void) { return 0x1400080020000UL; }
unsigned long foo_0x140080002000 (void) { return 0x140080002000UL; }
unsigned long foo_0x1800000120000000 (void) { return 0x1800000120000000UL; }
unsigned long foo_0x180000102000000 (void) { return 0x180000102000000UL; }
unsigned long foo_0x180000801 (void) { return 0x180000801UL; }
unsigned long foo_0x180000802 (void) { return 0x180000802UL; }
unsigned long foo_0x180001001 (void) { return 0x180001001UL; }
unsigned long foo_0x18000100200000 (void) { return 0x18000100200000UL; }
unsigned long foo_0x180001002 (void) { return 0x180001002UL; }
unsigned long foo_0x180003000 (void) { return 0x180003000UL; }
unsigned long foo_0x1800100020000 (void) { return 0x1800100020000UL; }
unsigned long foo_0x180081000 (void) { return 0x180081000UL; }
unsigned long foo_0x182001000 (void) { return 0x182001000UL; }
unsigned long foo_0x2400000240000000 (void) { return 0x2400000240000000UL; }
unsigned long foo_0x24000080400000 (void) { return 0x24000080400000UL; }
unsigned long foo_0x2400200040000 (void) { return 0x2400200040000UL; }
unsigned long foo_0x2800000140000000 (void) { return 0x2800000140000000UL; }
unsigned long foo_0x280000104000000 (void) { return 0x280000104000000UL; }
unsigned long foo_0x28000100400000 (void) { return 0x28000100400000UL; }
unsigned long foo_0x2800100040000 (void) { return 0x2800100040000UL; }
unsigned long foo_0x280011000 (void) { return 0x280011000UL; }
unsigned long foo_0x280100004000 (void) { return 0x280100004000UL; }
unsigned long foo_0x280401000 (void) { return 0x280401000UL; }
unsigned long foo_0x290001000 (void) { return 0x290001000UL; }
unsigned long foo_0x30000000c0000000 (void) { return 0x30000000c0000000UL; }
unsigned long foo_0x300000084000000 (void) { return 0x300000084000000UL; }
unsigned long foo_0x300000801 (void) { return 0x300000801UL; }
unsigned long foo_0x30000080400000 (void) { return 0x30000080400000UL; }
unsigned long foo_0x300002004 (void) { return 0x300002004UL; }
unsigned long foo_0x300006000 (void) { return 0x300006000UL; }
unsigned long foo_0x3000080040000 (void) { return 0x3000080040000UL; }
unsigned long foo_0x300021000 (void) { return 0x300021000UL; }
unsigned long foo_0x300080004000 (void) { return 0x300080004000UL; }
unsigned long foo_0x300102000 (void) { return 0x300102000UL; }
unsigned long foo_0x300801000 (void) { return 0x300801000UL; }
unsigned long foo_0x304002000 (void) { return 0x304002000UL; }
unsigned long foo_0x320001000 (void) { return 0x320001000UL; }
unsigned long foo_0x4800000480000000 (void) { return 0x4800000480000000UL; }
unsigned long foo_0x48000100800000 (void) { return 0x48000100800000UL; }
unsigned long foo_0x4800400080000 (void) { return 0x4800400080000UL; }
unsigned long foo_0x5000000280000000 (void) { return 0x5000000280000000UL; }
unsigned long foo_0x500000208000000 (void) { return 0x500000208000000UL; }
unsigned long foo_0x50000200800000 (void) { return 0x50000200800000UL; }
unsigned long foo_0x5000200080000 (void) { return 0x5000200080000UL; }
unsigned long foo_0x500200008000 (void) { return 0x500200008000UL; }
unsigned long foo_0x6000000180000000 (void) { return 0x6000000180000000UL; }
unsigned long foo_0x600000108000000 (void) { return 0x600000108000000UL; }
unsigned long foo_0x60000100800000 (void) { return 0x60000100800000UL; }
unsigned long foo_0x6000100080000 (void) { return 0x6000100080000UL; }
unsigned long foo_0x600100008000 (void) { return 0x600100008000UL; }
unsigned long foo_0x900000090000000 (void) { return 0x900000090000000UL; }
unsigned long foo_0x90000201000000 (void) { return 0x90000201000000UL; }
unsigned long foo_0x900080010000 (void) { return 0x900080010000UL; }
unsigned long foo_0x90200001000 (void) { return 0x90200001000UL; }
unsigned long foo_0xc00000090000000 (void) { return 0xc00000090000000UL; }
unsigned long foo_0xc0000081000000 (void) { return 0xc0000081000000UL; }
unsigned long foo_0xc000080100000 (void) { return 0xc000080100000UL; }
unsigned long foo_0xc00080010000 (void) { return 0xc00080010000UL; }
unsigned long foo_0xc0080001000 (void) { return 0xc0080001000UL; }
