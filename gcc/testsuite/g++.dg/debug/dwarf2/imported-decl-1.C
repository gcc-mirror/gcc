// PR debug/39524
// { dg-do compile }
// { dg-options "-gdwarf-2 -dA -O0 -fno-merge-debug-strings" }

namespace A
{
  static int var2 = 2;
}

int
func ()
{
  using A::var2;
  return var2;
}

// { dg-final { scan-assembler-times "var2\[^\n\r\]*DW_AT_name" 1 } }
