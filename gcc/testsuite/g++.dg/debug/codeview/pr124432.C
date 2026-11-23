// PR debug/124432
// { dg-do compile }
// { dg-options "-std=c++17 -O0 -gcodeview" }

// write_fbreg_variable must not dereference a missing DW_AT_name for the
// artificial structured-binding variable.

struct S
{
  int a, b;
};

void
f ()
{
  auto [a, b] = S {};
}
