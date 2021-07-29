// PR c++/49673: check that test_data goes into .rodata
// { dg-do compile { target c++11 } }
// { dg-additional-options -G0 { target { { alpha*-*-* frv*-*-* ia64-*-* lm32*-*-* m32r*-*-* microblaze*-*-* mips*-*-* loongarch*-*-* nios2-*-* powerpc*-*-* rs6000*-*-* } && { ! { *-*-darwin* *-*-aix* alpha*-*-*vms* } } } } }
// { dg-final { scan-assembler "\\.rdata" { target mips*-*-* } } }
// { dg-final { scan-assembler "rodata" { target { { *-*-linux-gnu *-*-gnu* *-*-elf } && { ! { mips*-*-* riscv*-*-* } } } } } }

struct Data
{
  int i;
  constexpr Data(int i = 0) : i(i+1) {}
};

extern const Data test_data = { 1 };
