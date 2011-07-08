// PR c++/49673: check that test_data goes into .rodata
// { dg-options -std=c++0x }
// { dg-final { scan-assembler "rodata" { target { *-*-linux-gnu || *-*-elf } } } }

struct Data
{
  int i;
  constexpr Data(int i = 0) : i(i+1) {}
};

extern const Data test_data = { 1 };
