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
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|ret|sh1add|sh2add|sh3add|slli)" 556 } } */

unsigned long foo_0x80180001000(void) { return 0x80180001000UL; }

unsigned long foo_0x80280001000(void) { return 0x80280001000UL; }

unsigned long foo_0x80480001000(void) { return 0x80480001000UL; }

unsigned long foo_0x80880001000(void) { return 0x80880001000UL; }

unsigned long foo_0x81080001000(void) { return 0x81080001000UL; }

unsigned long foo_0x82080001000(void) { return 0x82080001000UL; }

unsigned long foo_0x84080001000(void) { return 0x84080001000UL; }

unsigned long foo_0x88080001000(void) { return 0x88080001000UL; }

unsigned long foo_0x90080001000(void) { return 0x90080001000UL; }

unsigned long foo_0xa0080001000(void) { return 0xa0080001000UL; }

unsigned long foo_0x80300001000(void) { return 0x80300001000UL; }

unsigned long foo_0x80500001000(void) { return 0x80500001000UL; }

unsigned long foo_0x80900001000(void) { return 0x80900001000UL; }

unsigned long foo_0x81100001000(void) { return 0x81100001000UL; }

unsigned long foo_0x82100001000(void) { return 0x82100001000UL; }

unsigned long foo_0x84100001000(void) { return 0x84100001000UL; }

unsigned long foo_0x88100001000(void) { return 0x88100001000UL; }

unsigned long foo_0x90100001000(void) { return 0x90100001000UL; }

unsigned long foo_0xa0100001000(void) { return 0xa0100001000UL; }

unsigned long foo_0xc0100001000(void) { return 0xc0100001000UL; }

unsigned long foo_0x80600001000(void) { return 0x80600001000UL; }

unsigned long foo_0x80a00001000(void) { return 0x80a00001000UL; }

unsigned long foo_0x81200001000(void) { return 0x81200001000UL; }

unsigned long foo_0x82200001000(void) { return 0x82200001000UL; }

unsigned long foo_0x84200001000(void) { return 0x84200001000UL; }

unsigned long foo_0x88200001000(void) { return 0x88200001000UL; }

unsigned long foo_0xa0200001000(void) { return 0xa0200001000UL; }

unsigned long foo_0xc0200001000(void) { return 0xc0200001000UL; }

unsigned long foo_0x80c00001000(void) { return 0x80c00001000UL; }

unsigned long foo_0x81400001000(void) { return 0x81400001000UL; }

unsigned long foo_0x82400001000(void) { return 0x82400001000UL; }

unsigned long foo_0x84400001000(void) { return 0x84400001000UL; }

unsigned long foo_0x88400001000(void) { return 0x88400001000UL; }

unsigned long foo_0x90400001000(void) { return 0x90400001000UL; }

unsigned long foo_0xa0400001000(void) { return 0xa0400001000UL; }

unsigned long foo_0xc0400001000(void) { return 0xc0400001000UL; }

unsigned long foo_0x81800001000(void) { return 0x81800001000UL; }

unsigned long foo_0x82800001000(void) { return 0x82800001000UL; }

unsigned long foo_0x84800001000(void) { return 0x84800001000UL; }

unsigned long foo_0x88800001000(void) { return 0x88800001000UL; }

unsigned long foo_0x90800001000(void) { return 0x90800001000UL; }

unsigned long foo_0xa0800001000(void) { return 0xa0800001000UL; }

unsigned long foo_0xc0800001000(void) { return 0xc0800001000UL; }

unsigned long foo_0x83000001000(void) { return 0x83000001000UL; }

unsigned long foo_0x85000001000(void) { return 0x85000001000UL; }

unsigned long foo_0x89000001000(void) { return 0x89000001000UL; }

unsigned long foo_0x91000001000(void) { return 0x91000001000UL; }

unsigned long foo_0xa1000001000(void) { return 0xa1000001000UL; }

unsigned long foo_0xc1000001000(void) { return 0xc1000001000UL; }

unsigned long foo_0x86000001000(void) { return 0x86000001000UL; }

unsigned long foo_0x8a000001000(void) { return 0x8a000001000UL; }

unsigned long foo_0x92000001000(void) { return 0x92000001000UL; }

unsigned long foo_0xa2000001000(void) { return 0xa2000001000UL; }

unsigned long foo_0xc2000001000(void) { return 0xc2000001000UL; }

unsigned long foo_0x8c000001000(void) { return 0x8c000001000UL; }

unsigned long foo_0x94000001000(void) { return 0x94000001000UL; }

unsigned long foo_0xa4000001000(void) { return 0xa4000001000UL; }

unsigned long foo_0xc4000001000(void) { return 0xc4000001000UL; }

unsigned long foo_0x98000001000(void) { return 0x98000001000UL; }

unsigned long foo_0xa8000001000(void) { return 0xa8000001000UL; }

unsigned long foo_0xc8000001000(void) { return 0xc8000001000UL; }

unsigned long foo_0xb0000001000(void) { return 0xb0000001000UL; }

unsigned long foo_0xd0000001000(void) { return 0xd0000001000UL; }

unsigned long foo_0xe0000001000(void) { return 0xe0000001000UL; }

unsigned long foo_0x100180002000(void) { return 0x100180002000UL; }

unsigned long foo_0x100280002000(void) { return 0x100280002000UL; }

unsigned long foo_0x100480002000(void) { return 0x100480002000UL; }

unsigned long foo_0x100880002000(void) { return 0x100880002000UL; }

unsigned long foo_0x101080002000(void) { return 0x101080002000UL; }

unsigned long foo_0x102080002000(void) { return 0x102080002000UL; }

unsigned long foo_0x104080002000(void) { return 0x104080002000UL; }

unsigned long foo_0x108080002000(void) { return 0x108080002000UL; }

unsigned long foo_0x110080002000(void) { return 0x110080002000UL; }

unsigned long foo_0x120080002000(void) { return 0x120080002000UL; }

unsigned long foo_0x180080002000(void) { return 0x180080002000UL; }

unsigned long foo_0x100300002000(void) { return 0x100300002000UL; }

unsigned long foo_0x100500002000(void) { return 0x100500002000UL; }

unsigned long foo_0x100900002000(void) { return 0x100900002000UL; }

unsigned long foo_0x101100002000(void) { return 0x101100002000UL; }

unsigned long foo_0x102100002000(void) { return 0x102100002000UL; }

unsigned long foo_0x104100002000(void) { return 0x104100002000UL; }

unsigned long foo_0x108100002000(void) { return 0x108100002000UL; }

unsigned long foo_0x110100002000(void) { return 0x110100002000UL; }

unsigned long foo_0x120100002000(void) { return 0x120100002000UL; }

unsigned long foo_0x140100002000(void) { return 0x140100002000UL; }

unsigned long foo_0x100600002000(void) { return 0x100600002000UL; }

unsigned long foo_0x100a00002000(void) { return 0x100a00002000UL; }

unsigned long foo_0x101200002000(void) { return 0x101200002000UL; }

unsigned long foo_0x102200002000(void) { return 0x102200002000UL; }

unsigned long foo_0x104200002000(void) { return 0x104200002000UL; }

unsigned long foo_0x108200002000(void) { return 0x108200002000UL; }

unsigned long foo_0x110200002000(void) { return 0x110200002000UL; }

unsigned long foo_0x120200002000(void) { return 0x120200002000UL; }

unsigned long foo_0x140200002000(void) { return 0x140200002000UL; }

unsigned long foo_0x180200002000(void) { return 0x180200002000UL; }

unsigned long foo_0x100c00002000(void) { return 0x100c00002000UL; }

unsigned long foo_0x101400002000(void) { return 0x101400002000UL; }

unsigned long foo_0x102400002000(void) { return 0x102400002000UL; }

unsigned long foo_0x104400002000(void) { return 0x104400002000UL; }

unsigned long foo_0x108400002000(void) { return 0x108400002000UL; }

unsigned long foo_0x110400002000(void) { return 0x110400002000UL; }

unsigned long foo_0x140400002000(void) { return 0x140400002000UL; }

unsigned long foo_0x180400002000(void) { return 0x180400002000UL; }

unsigned long foo_0x101800002000(void) { return 0x101800002000UL; }

unsigned long foo_0x102800002000(void) { return 0x102800002000UL; }

unsigned long foo_0x104800002000(void) { return 0x104800002000UL; }

unsigned long foo_0x108800002000(void) { return 0x108800002000UL; }

unsigned long foo_0x110800002000(void) { return 0x110800002000UL; }

unsigned long foo_0x120800002000(void) { return 0x120800002000UL; }

unsigned long foo_0x140800002000(void) { return 0x140800002000UL; }

unsigned long foo_0x180800002000(void) { return 0x180800002000UL; }

unsigned long foo_0x103000002000(void) { return 0x103000002000UL; }

unsigned long foo_0x105000002000(void) { return 0x105000002000UL; }

unsigned long foo_0x109000002000(void) { return 0x109000002000UL; }

unsigned long foo_0x111000002000(void) { return 0x111000002000UL; }

unsigned long foo_0x121000002000(void) { return 0x121000002000UL; }

unsigned long foo_0x141000002000(void) { return 0x141000002000UL; }

unsigned long foo_0x181000002000(void) { return 0x181000002000UL; }

unsigned long foo_0x106000002000(void) { return 0x106000002000UL; }

unsigned long foo_0x10a000002000(void) { return 0x10a000002000UL; }

unsigned long foo_0x112000002000(void) { return 0x112000002000UL; }

unsigned long foo_0x122000002000(void) { return 0x122000002000UL; }

unsigned long foo_0x142000002000(void) { return 0x142000002000UL; }

unsigned long foo_0x182000002000(void) { return 0x182000002000UL; }

unsigned long foo_0x10c000002000(void) { return 0x10c000002000UL; }

unsigned long foo_0x114000002000(void) { return 0x114000002000UL; }

unsigned long foo_0x124000002000(void) { return 0x124000002000UL; }

unsigned long foo_0x144000002000(void) { return 0x144000002000UL; }

unsigned long foo_0x184000002000(void) { return 0x184000002000UL; }

unsigned long foo_0x118000002000(void) { return 0x118000002000UL; }

unsigned long foo_0x128000002000(void) { return 0x128000002000UL; }

unsigned long foo_0x148000002000(void) { return 0x148000002000UL; }

unsigned long foo_0x188000002000(void) { return 0x188000002000UL; }

unsigned long foo_0x130000002000(void) { return 0x130000002000UL; }

unsigned long foo_0x150000002000(void) { return 0x150000002000UL; }

unsigned long foo_0x190000002000(void) { return 0x190000002000UL; }

unsigned long foo_0x160000002000(void) { return 0x160000002000UL; }

unsigned long foo_0x1a0000002000(void) { return 0x1a0000002000UL; }

unsigned long foo_0x1c0000002000(void) { return 0x1c0000002000UL; }
