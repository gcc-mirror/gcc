// PR debug/124432
// { dg-do compile }
// { dg-options "-std=c++17 -O2 -gcodeview" }

// write_optimized_local_variable must omit the unnamed structured-binding
// variable and its location range, retaining only those for a.

struct S
{
  int a;
};

void
f ()
{
  auto [a] = S {};
  asm volatile ("" : "+m" (a));
}

// One S_LOCAL and one S_DEFRANGE_REGISTER_REL for a.
// { dg-final { scan-assembler-times "0x113e" 1 } }
// { dg-final { scan-assembler-times "0x1145" 1 } }
