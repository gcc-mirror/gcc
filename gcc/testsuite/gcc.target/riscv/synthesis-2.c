/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gcb" } */

/* All these cases have the same form.  lui+slli.uw+addi
/* { dg-final { scan-assembler-times "\\tadd" 367 } } */
/* { dg-final { scan-assembler-times "\\tslli.uw" 367 } } */
/* { dg-final { scan-assembler-times "\\taddi" 367 } } */

unsigned long foo_0x480008001(void) {
    return 0x480008001UL;
}

unsigned long foo_0x1080020001(void) {
    return 0x1080020001UL;
}

unsigned long foo_0x2800040001(void) {
    return 0x2800040001UL;
}

unsigned long foo_0x4080080001(void) {
    return 0x4080080001UL;
}

unsigned long foo_0x6000080001(void) {
    return 0x6000080001UL;
}

unsigned long foo_0x8800100001(void) {
    return 0x8800100001UL;
}

unsigned long foo_0x10080200001(void) {
    return 0x10080200001UL;
}

unsigned long foo_0x12000200001(void) {
    return 0x12000200001UL;
}

unsigned long foo_0x20800400001(void) {
    return 0x20800400001UL;
}

unsigned long foo_0x40080800001(void) {
    return 0x40080800001UL;
}

unsigned long foo_0x42000800001(void) {
    return 0x42000800001UL;
}

unsigned long foo_0x80801000001(void) {
    return 0x80801000001UL;
}

unsigned long foo_0xa0001000001(void) {
    return 0xa0001000001UL;
}

unsigned long foo_0x100082000001(void) {
    return 0x100082000001UL;
}

unsigned long foo_0x102002000001(void) {
    return 0x102002000001UL;
}

unsigned long foo_0x180002000001(void) {
    return 0x180002000001UL;
}

unsigned long foo_0x200804000001(void) {
    return 0x200804000001UL;
}

unsigned long foo_0x220004000001(void) {
    return 0x220004000001UL;
}

unsigned long foo_0x400088000001(void) {
    return 0x400088000001UL;
}

unsigned long foo_0x402008000001(void) {
    return 0x402008000001UL;
}

unsigned long foo_0x480008000001(void) {
    return 0x480008000001UL;
}

unsigned long foo_0x800810000001(void) {
    return 0x800810000001UL;
}

unsigned long foo_0x820010000001(void) {
    return 0x820010000001UL;
}

unsigned long foo_0x10000a0000001(void) {
    return 0x10000a0000001UL;
}

unsigned long foo_0x1002020000001(void) {
    return 0x1002020000001UL;
}

unsigned long foo_0x1080020000001(void) {
    return 0x1080020000001UL;
}

unsigned long foo_0x2000840000001(void) {
    return 0x2000840000001UL;
}

unsigned long foo_0x2020040000001(void) {
    return 0x2020040000001UL;
}

unsigned long foo_0x2800040000001(void) {
    return 0x2800040000001UL;
}

unsigned long foo_0x4002080000001(void) {
    return 0x4002080000001UL;
}

unsigned long foo_0x4080080000001(void) {
    return 0x4080080000001UL;
}

unsigned long foo_0x6000080000001(void) {
    return 0x6000080000001UL;
}

unsigned long foo_0x8000900000001(void) {
    return 0x8000900000001UL;
}

unsigned long foo_0x8020100000001(void) {
    return 0x8020100000001UL;
}

unsigned long foo_0x8800100000001(void) {
    return 0x8800100000001UL;
}

unsigned long foo_0x10002200000001(void) {
    return 0x10002200000001UL;
}

unsigned long foo_0x10080200000001(void) {
    return 0x10080200000001UL;
}

unsigned long foo_0x12000200000001(void) {
    return 0x12000200000001UL;
}

unsigned long foo_0x20000c00000001(void) {
    return 0x20000c00000001UL;
}

unsigned long foo_0x20020400000001(void) {
    return 0x20020400000001UL;
}

unsigned long foo_0x20800400000001(void) {
    return 0x20800400000001UL;
}

unsigned long foo_0x40002800000001(void) {
    return 0x40002800000001UL;
}

unsigned long foo_0x40080800000001(void) {
    return 0x40080800000001UL;
}

unsigned long foo_0x42000800000001(void) {
    return 0x42000800000001UL;
}

unsigned long foo_0x80021000000001(void) {
    return 0x80021000000001UL;
}

unsigned long foo_0x80801000000001(void) {
    return 0x80801000000001UL;
}

unsigned long foo_0xa0001000000001(void) {
    return 0xa0001000000001UL;
}

unsigned long foo_0x100082000000001(void) {
    return 0x100082000000001UL;
}

unsigned long foo_0x102002000000001(void) {
    return 0x102002000000001UL;
}

unsigned long foo_0x180002000000001(void) {
    return 0x180002000000001UL;
}

unsigned long foo_0x200024000000001(void) {
    return 0x200024000000001UL;
}

unsigned long foo_0x200804000000001(void) {
    return 0x200804000000001UL;
}

unsigned long foo_0x220004000000001(void) {
    return 0x220004000000001UL;
}

unsigned long foo_0x400088000000001(void) {
    return 0x400088000000001UL;
}

unsigned long foo_0x402008000000001(void) {
    return 0x402008000000001UL;
}

unsigned long foo_0x480008000000001(void) {
    return 0x480008000000001UL;
}

unsigned long foo_0x800030000000001(void) {
    return 0x800030000000001UL;
}

unsigned long foo_0x800810000000001(void) {
    return 0x800810000000001UL;
}

unsigned long foo_0x820010000000001(void) {
    return 0x820010000000001UL;
}

unsigned long foo_0x10000a0000000001(void) {
    return 0x10000a0000000001UL;
}

unsigned long foo_0x1002020000000001(void) {
    return 0x1002020000000001UL;
}

unsigned long foo_0x1080020000000001(void) {
    return 0x1080020000000001UL;
}

unsigned long foo_0x2000840000000001(void) {
    return 0x2000840000000001UL;
}

unsigned long foo_0x2020040000000001(void) {
    return 0x2020040000000001UL;
}

unsigned long foo_0x2800040000000001(void) {
    return 0x2800040000000001UL;
}

unsigned long foo_0x4002080000000001(void) {
    return 0x4002080000000001UL;
}

unsigned long foo_0x4080080000000001(void) {
    return 0x4080080000000001UL;
}

unsigned long foo_0x6000080000000001(void) {
    return 0x6000080000000001UL;
}

unsigned long foo_0x900010002(void) {
    return 0x900010002UL;
}

unsigned long foo_0x2100040002(void) {
    return 0x2100040002UL;
}

unsigned long foo_0x5000080002(void) {
    return 0x5000080002UL;
}

unsigned long foo_0x8100100002(void) {
    return 0x8100100002UL;
}

unsigned long foo_0xc000100002(void) {
    return 0xc000100002UL;
}

unsigned long foo_0x11000200002(void) {
    return 0x11000200002UL;
}

unsigned long foo_0x20100400002(void) {
    return 0x20100400002UL;
}

unsigned long foo_0x24000400002(void) {
    return 0x24000400002UL;
}

unsigned long foo_0x41000800002(void) {
    return 0x41000800002UL;
}

unsigned long foo_0x80101000002(void) {
    return 0x80101000002UL;
}

unsigned long foo_0x84001000002(void) {
    return 0x84001000002UL;
}

unsigned long foo_0x101002000002(void) {
    return 0x101002000002UL;
}

unsigned long foo_0x140002000002(void) {
    return 0x140002000002UL;
}

unsigned long foo_0x200104000002(void) {
    return 0x200104000002UL;
}

unsigned long foo_0x204004000002(void) {
    return 0x204004000002UL;
}

unsigned long foo_0x300004000002(void) {
    return 0x300004000002UL;
}

unsigned long foo_0x401008000002(void) {
    return 0x401008000002UL;
}

unsigned long foo_0x440008000002(void) {
    return 0x440008000002UL;
}

unsigned long foo_0x800110000002(void) {
    return 0x800110000002UL;
}

unsigned long foo_0x804010000002(void) {
    return 0x804010000002UL;
}

unsigned long foo_0x900010000002(void) {
    return 0x900010000002UL;
}

unsigned long foo_0x1001020000002(void) {
    return 0x1001020000002UL;
}

unsigned long foo_0x1040020000002(void) {
    return 0x1040020000002UL;
}

unsigned long foo_0x2000140000002(void) {
    return 0x2000140000002UL;
}

unsigned long foo_0x2004040000002(void) {
    return 0x2004040000002UL;
}

unsigned long foo_0x2100040000002(void) {
    return 0x2100040000002UL;
}

unsigned long foo_0x4001080000002(void) {
    return 0x4001080000002UL;
}

unsigned long foo_0x4040080000002(void) {
    return 0x4040080000002UL;
}

unsigned long foo_0x5000080000002(void) {
    return 0x5000080000002UL;
}

unsigned long foo_0x8004100000002(void) {
    return 0x8004100000002UL;
}

unsigned long foo_0x8100100000002(void) {
    return 0x8100100000002UL;
}

unsigned long foo_0xc000100000002(void) {
    return 0xc000100000002UL;
}

unsigned long foo_0x10001200000002(void) {
    return 0x10001200000002UL;
}

unsigned long foo_0x10040200000002(void) {
    return 0x10040200000002UL;
}

unsigned long foo_0x11000200000002(void) {
    return 0x11000200000002UL;
}

unsigned long foo_0x20004400000002(void) {
    return 0x20004400000002UL;
}

unsigned long foo_0x20100400000002(void) {
    return 0x20100400000002UL;
}

unsigned long foo_0x24000400000002(void) {
    return 0x24000400000002UL;
}

unsigned long foo_0x40001800000002(void) {
    return 0x40001800000002UL;
}

unsigned long foo_0x40040800000002(void) {
    return 0x40040800000002UL;
}

unsigned long foo_0x41000800000002(void) {
    return 0x41000800000002UL;
}

unsigned long foo_0x80005000000002(void) {
    return 0x80005000000002UL;
}

unsigned long foo_0x80101000000002(void) {
    return 0x80101000000002UL;
}

unsigned long foo_0x84001000000002(void) {
    return 0x84001000000002UL;
}

unsigned long foo_0x100042000000002(void) {
    return 0x100042000000002UL;
}

unsigned long foo_0x101002000000002(void) {
    return 0x101002000000002UL;
}

unsigned long foo_0x140002000000002(void) {
    return 0x140002000000002UL;
}

unsigned long foo_0x200104000000002(void) {
    return 0x200104000000002UL;
}

unsigned long foo_0x204004000000002(void) {
    return 0x204004000000002UL;
}

unsigned long foo_0x300004000000002(void) {
    return 0x300004000000002UL;
}

unsigned long foo_0x400048000000002(void) {
    return 0x400048000000002UL;
}

unsigned long foo_0x401008000000002(void) {
    return 0x401008000000002UL;
}

unsigned long foo_0x440008000000002(void) {
    return 0x440008000000002UL;
}

unsigned long foo_0x800110000000002(void) {
    return 0x800110000000002UL;
}

unsigned long foo_0x804010000000002(void) {
    return 0x804010000000002UL;
}

unsigned long foo_0x900010000000002(void) {
    return 0x900010000000002UL;
}

unsigned long foo_0x1000060000000002(void) {
    return 0x1000060000000002UL;
}

unsigned long foo_0x1001020000000002(void) {
    return 0x1001020000000002UL;
}

unsigned long foo_0x1040020000000002(void) {
    return 0x1040020000000002UL;
}

unsigned long foo_0x2000140000000002(void) {
    return 0x2000140000000002UL;
}

unsigned long foo_0x2004040000000002(void) {
    return 0x2004040000000002UL;
}

unsigned long foo_0x2100040000000002(void) {
    return 0x2100040000000002UL;
}

unsigned long foo_0x4001080000000002(void) {
    return 0x4001080000000002UL;
}

unsigned long foo_0x4040080000000002(void) {
    return 0x4040080000000002UL;
}

unsigned long foo_0x5000080000000002(void) {
    return 0x5000080000000002UL;
}

unsigned long foo_0x600008004(void) {
    return 0x600008004UL;
}

unsigned long foo_0x880010004(void) {
    return 0x880010004UL;
}

unsigned long foo_0x1200020004(void) {
    return 0x1200020004UL;
}

unsigned long foo_0x2080040004(void) {
    return 0x2080040004UL;
}

unsigned long foo_0x4200080004(void) {
    return 0x4200080004UL;
}

unsigned long foo_0x8080100004(void) {
    return 0x8080100004UL;
}

unsigned long foo_0xa000100004(void) {
    return 0xa000100004UL;
}

unsigned long foo_0x10200200004(void) {
    return 0x10200200004UL;
}

unsigned long foo_0x18000200004(void) {
    return 0x18000200004UL;
}

unsigned long foo_0x20080400004(void) {
    return 0x20080400004UL;
}

unsigned long foo_0x22000400004(void) {
    return 0x22000400004UL;
}

unsigned long foo_0x40200800004(void) {
    return 0x40200800004UL;
}

unsigned long foo_0x48000800004(void) {
    return 0x48000800004UL;
}

unsigned long foo_0x80081000004(void) {
    return 0x80081000004UL;
}

unsigned long foo_0x82001000004(void) {
    return 0x82001000004UL;
}

unsigned long foo_0x100202000004(void) {
    return 0x100202000004UL;
}

unsigned long foo_0x108002000004(void) {
    return 0x108002000004UL;
}

unsigned long foo_0x200084000004(void) {
    return 0x200084000004UL;
}

unsigned long foo_0x202004000004(void) {
    return 0x202004000004UL;
}

unsigned long foo_0x280004000004(void) {
    return 0x280004000004UL;
}

unsigned long foo_0x400208000004(void) {
    return 0x400208000004UL;
}

unsigned long foo_0x408008000004(void) {
    return 0x408008000004UL;
}

unsigned long foo_0x600008000004(void) {
    return 0x600008000004UL;
}

unsigned long foo_0x800090000004(void) {
    return 0x800090000004UL;
}

unsigned long foo_0x802010000004(void) {
    return 0x802010000004UL;
}

unsigned long foo_0x880010000004(void) {
    return 0x880010000004UL;
}

unsigned long foo_0x1000220000004(void) {
    return 0x1000220000004UL;
}

unsigned long foo_0x1008020000004(void) {
    return 0x1008020000004UL;
}

unsigned long foo_0x1200020000004(void) {
    return 0x1200020000004UL;
}

unsigned long foo_0x20000c0000004(void) {
    return 0x20000c0000004UL;
}

unsigned long foo_0x2002040000004(void) {
    return 0x2002040000004UL;
}

unsigned long foo_0x2080040000004(void) {
    return 0x2080040000004UL;
}

unsigned long foo_0x4000280000004(void) {
    return 0x4000280000004UL;
}

unsigned long foo_0x4008080000004(void) {
    return 0x4008080000004UL;
}

unsigned long foo_0x4200080000004(void) {
    return 0x4200080000004UL;
}

unsigned long foo_0x8002100000004(void) {
    return 0x8002100000004UL;
}

unsigned long foo_0x8080100000004(void) {
    return 0x8080100000004UL;
}

unsigned long foo_0xa000100000004(void) {
    return 0xa000100000004UL;
}

unsigned long foo_0x10008200000004(void) {
    return 0x10008200000004UL;
}

unsigned long foo_0x10200200000004(void) {
    return 0x10200200000004UL;
}

unsigned long foo_0x18000200000004(void) {
    return 0x18000200000004UL;
}

unsigned long foo_0x20002400000004(void) {
    return 0x20002400000004UL;
}

unsigned long foo_0x20080400000004(void) {
    return 0x20080400000004UL;
}

unsigned long foo_0x22000400000004(void) {
    return 0x22000400000004UL;
}

unsigned long foo_0x40008800000004(void) {
    return 0x40008800000004UL;
}

unsigned long foo_0x40200800000004(void) {
    return 0x40200800000004UL;
}

unsigned long foo_0x48000800000004(void) {
    return 0x48000800000004UL;
}

unsigned long foo_0x80003000000004(void) {
    return 0x80003000000004UL;
}

unsigned long foo_0x80081000000004(void) {
    return 0x80081000000004UL;
}

unsigned long foo_0x82001000000004(void) {
    return 0x82001000000004UL;
}

unsigned long foo_0x10000a000000004(void) {
    return 0x10000a000000004UL;
}

unsigned long foo_0x100202000000004(void) {
    return 0x100202000000004UL;
}

unsigned long foo_0x108002000000004(void) {
    return 0x108002000000004UL;
}

unsigned long foo_0x200084000000004(void) {
    return 0x200084000000004UL;
}

unsigned long foo_0x202004000000004(void) {
    return 0x202004000000004UL;
}

unsigned long foo_0x280004000000004(void) {
    return 0x280004000000004UL;
}

unsigned long foo_0x400208000000004(void) {
    return 0x400208000000004UL;
}

unsigned long foo_0x408008000000004(void) {
    return 0x408008000000004UL;
}

unsigned long foo_0x600008000000004(void) {
    return 0x600008000000004UL;
}

unsigned long foo_0x800090000000004(void) {
    return 0x800090000000004UL;
}

unsigned long foo_0x802010000000004(void) {
    return 0x802010000000004UL;
}

unsigned long foo_0x880010000000004(void) {
    return 0x880010000000004UL;
}

unsigned long foo_0x1000220000000004(void) {
    return 0x1000220000000004UL;
}

unsigned long foo_0x1008020000000004(void) {
    return 0x1008020000000004UL;
}

unsigned long foo_0x1200020000000004(void) {
    return 0x1200020000000004UL;
}

unsigned long foo_0x20000c0000000004(void) {
    return 0x20000c0000000004UL;
}

unsigned long foo_0x2002040000000004(void) {
    return 0x2002040000000004UL;
}

unsigned long foo_0x2080040000000004(void) {
    return 0x2080040000000004UL;
}

unsigned long foo_0x4000280000000004(void) {
    return 0x4000280000000004UL;
}

unsigned long foo_0x4008080000000004(void) {
    return 0x4008080000000004UL;
}

unsigned long foo_0x4200080000000004(void) {
    return 0x4200080000000004UL;
}

unsigned long foo_0x500008008(void) {
    return 0x500008008UL;
}

unsigned long foo_0xc00010008(void) {
    return 0xc00010008UL;
}

unsigned long foo_0x1100020008(void) {
    return 0x1100020008UL;
}

unsigned long foo_0x2400040008(void) {
    return 0x2400040008UL;
}

unsigned long foo_0x4100080008(void) {
    return 0x4100080008UL;
}

unsigned long foo_0x8400100008(void) {
    return 0x8400100008UL;
}

unsigned long foo_0x10100200008(void) {
    return 0x10100200008UL;
}

unsigned long foo_0x14000200008(void) {
    return 0x14000200008UL;
}

unsigned long foo_0x20400400008(void) {
    return 0x20400400008UL;
}

unsigned long foo_0x30000400008(void) {
    return 0x30000400008UL;
}

unsigned long foo_0x40100800008(void) {
    return 0x40100800008UL;
}

unsigned long foo_0x44000800008(void) {
    return 0x44000800008UL;
}

unsigned long foo_0x80401000008(void) {
    return 0x80401000008UL;
}

unsigned long foo_0x90001000008(void) {
    return 0x90001000008UL;
}

unsigned long foo_0x100102000008(void) {
    return 0x100102000008UL;
}

unsigned long foo_0x104002000008(void) {
    return 0x104002000008UL;
}

unsigned long foo_0x200404000008(void) {
    return 0x200404000008UL;
}

unsigned long foo_0x210004000008(void) {
    return 0x210004000008UL;
}

unsigned long foo_0x400108000008(void) {
    return 0x400108000008UL;
}

unsigned long foo_0x404008000008(void) {
    return 0x404008000008UL;
}

unsigned long foo_0x500008000008(void) {
    return 0x500008000008UL;
}

unsigned long foo_0x800410000008(void) {
    return 0x800410000008UL;
}

unsigned long foo_0x810010000008(void) {
    return 0x810010000008UL;
}

unsigned long foo_0xc00010000008(void) {
    return 0xc00010000008UL;
}

unsigned long foo_0x1000120000008(void) {
    return 0x1000120000008UL;
}

unsigned long foo_0x1004020000008(void) {
    return 0x1004020000008UL;
}

unsigned long foo_0x1100020000008(void) {
    return 0x1100020000008UL;
}

unsigned long foo_0x2000440000008(void) {
    return 0x2000440000008UL;
}

unsigned long foo_0x2010040000008(void) {
    return 0x2010040000008UL;
}

unsigned long foo_0x2400040000008(void) {
    return 0x2400040000008UL;
}

unsigned long foo_0x4000180000008(void) {
    return 0x4000180000008UL;
}

unsigned long foo_0x4004080000008(void) {
    return 0x4004080000008UL;
}

unsigned long foo_0x4100080000008(void) {
    return 0x4100080000008UL;
}

unsigned long foo_0x8000500000008(void) {
    return 0x8000500000008UL;
}

unsigned long foo_0x8010100000008(void) {
    return 0x8010100000008UL;
}

unsigned long foo_0x8400100000008(void) {
    return 0x8400100000008UL;
}

unsigned long foo_0x10004200000008(void) {
    return 0x10004200000008UL;
}

unsigned long foo_0x10100200000008(void) {
    return 0x10100200000008UL;
}

unsigned long foo_0x14000200000008(void) {
    return 0x14000200000008UL;
}

unsigned long foo_0x20010400000008(void) {
    return 0x20010400000008UL;
}

unsigned long foo_0x20400400000008(void) {
    return 0x20400400000008UL;
}

unsigned long foo_0x30000400000008(void) {
    return 0x30000400000008UL;
}

unsigned long foo_0x40004800000008(void) {
    return 0x40004800000008UL;
}

unsigned long foo_0x40100800000008(void) {
    return 0x40100800000008UL;
}

unsigned long foo_0x44000800000008(void) {
    return 0x44000800000008UL;
}

unsigned long foo_0x80011000000008(void) {
    return 0x80011000000008UL;
}

unsigned long foo_0x80401000000008(void) {
    return 0x80401000000008UL;
}

unsigned long foo_0x90001000000008(void) {
    return 0x90001000000008UL;
}

unsigned long foo_0x100006000000008(void) {
    return 0x100006000000008UL;
}

unsigned long foo_0x100102000000008(void) {
    return 0x100102000000008UL;
}

unsigned long foo_0x104002000000008(void) {
    return 0x104002000000008UL;
}

unsigned long foo_0x200014000000008(void) {
    return 0x200014000000008UL;
}

unsigned long foo_0x200404000000008(void) {
    return 0x200404000000008UL;
}

unsigned long foo_0x210004000000008(void) {
    return 0x210004000000008UL;
}

unsigned long foo_0x400108000000008(void) {
    return 0x400108000000008UL;
}

unsigned long foo_0x404008000000008(void) {
    return 0x404008000000008UL;
}

unsigned long foo_0x500008000000008(void) {
    return 0x500008000000008UL;
}

unsigned long foo_0x800410000000008(void) {
    return 0x800410000000008UL;
}

unsigned long foo_0x810010000000008(void) {
    return 0x810010000000008UL;
}

unsigned long foo_0xc00010000000008(void) {
    return 0xc00010000000008UL;
}

unsigned long foo_0x1000120000000008(void) {
    return 0x1000120000000008UL;
}

unsigned long foo_0x1004020000000008(void) {
    return 0x1004020000000008UL;
}

unsigned long foo_0x1100020000000008(void) {
    return 0x1100020000000008UL;
}

unsigned long foo_0x2000440000000008(void) {
    return 0x2000440000000008UL;
}

unsigned long foo_0x2010040000000008(void) {
    return 0x2010040000000008UL;
}

unsigned long foo_0x2400040000000008(void) {
    return 0x2400040000000008UL;
}

unsigned long foo_0x4000180000000008(void) {
    return 0x4000180000000008UL;
}

unsigned long foo_0x4004080000000008(void) {
    return 0x4004080000000008UL;
}

unsigned long foo_0x4100080000000008(void) {
    return 0x4100080000000008UL;
}

unsigned long foo_0xa00010010(void) {
    return 0xa00010010UL;
}

unsigned long foo_0x1800020010(void) {
    return 0x1800020010UL;
}

unsigned long foo_0x2200040010(void) {
    return 0x2200040010UL;
}

unsigned long foo_0x4800080010(void) {
    return 0x4800080010UL;
}

unsigned long foo_0x8200100010(void) {
    return 0x8200100010UL;
}

unsigned long foo_0x10800200010(void) {
    return 0x10800200010UL;
}

unsigned long foo_0x20200400010(void) {
    return 0x20200400010UL;
}

unsigned long foo_0x28000400010(void) {
    return 0x28000400010UL;
}

unsigned long foo_0x40800800010(void) {
    return 0x40800800010UL;
}

unsigned long foo_0x60000800010(void) {
    return 0x60000800010UL;
}

unsigned long foo_0x80201000010(void) {
    return 0x80201000010UL;
}

unsigned long foo_0x88001000010(void) {
    return 0x88001000010UL;
}

unsigned long foo_0x100802000010(void) {
    return 0x100802000010UL;
}

unsigned long foo_0x120002000010(void) {
    return 0x120002000010UL;
}

unsigned long foo_0x200204000010(void) {
    return 0x200204000010UL;
}

unsigned long foo_0x208004000010(void) {
    return 0x208004000010UL;
}

unsigned long foo_0x400808000010(void) {
    return 0x400808000010UL;
}

unsigned long foo_0x420008000010(void) {
    return 0x420008000010UL;
}

unsigned long foo_0x800210000010(void) {
    return 0x800210000010UL;
}

unsigned long foo_0x808010000010(void) {
    return 0x808010000010UL;
}

unsigned long foo_0xa00010000010(void) {
    return 0xa00010000010UL;
}

unsigned long foo_0x1000820000010(void) {
    return 0x1000820000010UL;
}

unsigned long foo_0x1020020000010(void) {
    return 0x1020020000010UL;
}

unsigned long foo_0x1800020000010(void) {
    return 0x1800020000010UL;
}

unsigned long foo_0x2000240000010(void) {
    return 0x2000240000010UL;
}

unsigned long foo_0x2008040000010(void) {
    return 0x2008040000010UL;
}

unsigned long foo_0x2200040000010(void) {
    return 0x2200040000010UL;
}

unsigned long foo_0x4000880000010(void) {
    return 0x4000880000010UL;
}

unsigned long foo_0x4020080000010(void) {
    return 0x4020080000010UL;
}

unsigned long foo_0x4800080000010(void) {
    return 0x4800080000010UL;
}

unsigned long foo_0x8000300000010(void) {
    return 0x8000300000010UL;
}

unsigned long foo_0x8008100000010(void) {
    return 0x8008100000010UL;
}

unsigned long foo_0x8200100000010(void) {
    return 0x8200100000010UL;
}

unsigned long foo_0x10000a00000010(void) {
    return 0x10000a00000010UL;
}

unsigned long foo_0x10020200000010(void) {
    return 0x10020200000010UL;
}

unsigned long foo_0x10800200000010(void) {
    return 0x10800200000010UL;
}

unsigned long foo_0x20008400000010(void) {
    return 0x20008400000010UL;
}

unsigned long foo_0x20200400000010(void) {
    return 0x20200400000010UL;
}

unsigned long foo_0x28000400000010(void) {
    return 0x28000400000010UL;
}

unsigned long foo_0x40020800000010(void) {
    return 0x40020800000010UL;
}

unsigned long foo_0x40800800000010(void) {
    return 0x40800800000010UL;
}

unsigned long foo_0x60000800000010(void) {
    return 0x60000800000010UL;
}

unsigned long foo_0x80009000000010(void) {
    return 0x80009000000010UL;
}

unsigned long foo_0x80201000000010(void) {
    return 0x80201000000010UL;
}

unsigned long foo_0x88001000000010(void) {
    return 0x88001000000010UL;
}

unsigned long foo_0x100022000000010(void) {
    return 0x100022000000010UL;
}

unsigned long foo_0x100802000000010(void) {
    return 0x100802000000010UL;
}

unsigned long foo_0x120002000000010(void) {
    return 0x120002000000010UL;
}

unsigned long foo_0x20000c000000010(void) {
    return 0x20000c000000010UL;
}

unsigned long foo_0x200204000000010(void) {
    return 0x200204000000010UL;
}

unsigned long foo_0x208004000000010(void) {
    return 0x208004000000010UL;
}

unsigned long foo_0x400028000000010(void) {
    return 0x400028000000010UL;
}

unsigned long foo_0x400808000000010(void) {
    return 0x400808000000010UL;
}

unsigned long foo_0x420008000000010(void) {
    return 0x420008000000010UL;
}

unsigned long foo_0x800210000000010(void) {
    return 0x800210000000010UL;
}

unsigned long foo_0x808010000000010(void) {
    return 0x808010000000010UL;
}

unsigned long foo_0xa00010000000010(void) {
    return 0xa00010000000010UL;
}

unsigned long foo_0x1000820000000010(void) {
    return 0x1000820000000010UL;
}

unsigned long foo_0x1020020000000010(void) {
    return 0x1020020000000010UL;
}

unsigned long foo_0x1800020000000010(void) {
    return 0x1800020000000010UL;
}

unsigned long foo_0x2000240000000010(void) {
    return 0x2000240000000010UL;
}

unsigned long foo_0x2008040000000010(void) {
    return 0x2008040000000010UL;
}

unsigned long foo_0x2200040000000010(void) {
    return 0x2200040000000010UL;
}

unsigned long foo_0x4000880000000010(void) {
    return 0x4000880000000010UL;
}

unsigned long foo_0x4020080000000010(void) {
    return 0x4020080000000010UL;
}

unsigned long foo_0x4800080000000010(void) {
    return 0x4800080000000010UL;
}

unsigned long foo_0x1400020020(void) {
    return 0x1400020020UL;
}

unsigned long foo_0x3000040020(void) {
    return 0x3000040020UL;
}

unsigned long foo_0x4400080020(void) {
    return 0x4400080020UL;
}

unsigned long foo_0x9000100020(void) {
    return 0x9000100020UL;
}

unsigned long foo_0x10400200020(void) {
    return 0x10400200020UL;
}

unsigned long foo_0x21000400020(void) {
    return 0x21000400020UL;
}

unsigned long foo_0x40400800020(void) {
    return 0x40400800020UL;
}

unsigned long foo_0x50000800020(void) {
    return 0x50000800020UL;
}

unsigned long foo_0x81001000020(void) {
    return 0x81001000020UL;
}

unsigned long foo_0xc0001000020(void) {
    return 0xc0001000020UL;
}

unsigned long foo_0x100402000020(void) {
    return 0x100402000020UL;
}

unsigned long foo_0x110002000020(void) {
    return 0x110002000020UL;
}

unsigned long foo_0x201004000020(void) {
    return 0x201004000020UL;
}

unsigned long foo_0x240004000020(void) {
    return 0x240004000020UL;
}

unsigned long foo_0x400408000020(void) {
    return 0x400408000020UL;
}

unsigned long foo_0x410008000020(void) {
    return 0x410008000020UL;
}

unsigned long foo_0x801010000020(void) {
    return 0x801010000020UL;
}

unsigned long foo_0x840010000020(void) {
    return 0x840010000020UL;
}

unsigned long foo_0x1000420000020(void) {
    return 0x1000420000020UL;
}

unsigned long foo_0x1010020000020(void) {
    return 0x1010020000020UL;
}

unsigned long foo_0x1400020000020(void) {
    return 0x1400020000020UL;
}

unsigned long foo_0x2001040000020(void) {
    return 0x2001040000020UL;
}

unsigned long foo_0x2040040000020(void) {
    return 0x2040040000020UL;
}

unsigned long foo_0x3000040000020(void) {
    return 0x3000040000020UL;
}

unsigned long foo_0x4000480000020(void) {
    return 0x4000480000020UL;
}

unsigned long foo_0x4010080000020(void) {
    return 0x4010080000020UL;
}

unsigned long foo_0x4400080000020(void) {
    return 0x4400080000020UL;
}

unsigned long foo_0x8001100000020(void) {
    return 0x8001100000020UL;
}
