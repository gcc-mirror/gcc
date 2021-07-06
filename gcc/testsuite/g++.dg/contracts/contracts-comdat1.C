// Contract condition functions should be local symbols in a comdat group with
// the guarded function.

// { dg-do compile { target { c++20 && comdat_group } } }
// { dg-additional-options -fcontracts }
// { dg-final { scan-assembler-not "_Z1fi.pre,comdat" } }
// { dg-final { scan-assembler-not {(weak|globl)[^\n]*_Z1fi.pre} } }

inline int f(int i)
  [[ pre: i > 0 ]]
{
  return i;
}

int main()
{
  if (f(42) != 42)
    __builtin_abort ();
}
