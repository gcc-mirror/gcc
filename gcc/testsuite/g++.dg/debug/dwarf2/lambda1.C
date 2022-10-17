// PR c++/43912
// { dg-do compile { target c++11 } }
// { dg-options "-gdwarf-2 -dA -fno-merge-debug-strings -gno-strict-dwarf -fno-inline" }

// Check for the local alias variables that point to the members of the closure.
// { dg-final { scan-assembler-times "DW_TAG_variable\[^.\]*\.ascii \"j.0\"" 4 { xfail { powerpc-ibm-aix* } } } }
// { dg-final { scan-assembler-times "DW_TAG_variable\[^.\]*\.ascii \"this.0\"" 2 { xfail { powerpc-ibm-aix* } } } }

struct A
{
  int i;
  int f()
  {
    int j;
    [&]() { j = i; }();
    return j;
  }
};

template <class T>
struct B
{
  int i;
  int f()
  {
    int j;
    [&]() { j = i; }();
    return j;
  }
};

int main()
{
  A().f();
  B<int>().f();
}
