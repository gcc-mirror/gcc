// PR middle-end/116896
// { dg-do compile { target c++20 } }
// { dg-options "-O2 -masm=att -fno-stack-protector" }
// { dg-final { scan-assembler-times "\tjp\t" 1 } }
// { dg-final { scan-assembler-not "\tj\[^mp\]\[a-z\]*\t" } }
// { dg-final { scan-assembler-times "\tsbb\[bl\]\t\\\$0, " 3 } }
// { dg-final { scan-assembler-times "\tseta\t" 3 } }
// { dg-final { scan-assembler-times "\tsetg\t" 1 } }
// { dg-final { scan-assembler-times "\tsetl\t" 1 } }

#include <compare>

[[gnu::noipa]] auto
foo (float x, float y)
{
  return x <=> y;
}

[[gnu::noipa, gnu::optimize ("fast-math")]] auto
bar (float x, float y)
{
  return x <=> y;
}

[[gnu::noipa]] auto
baz (int x, int y)
{
  return x <=> y;
}

[[gnu::noipa]] auto
qux (unsigned x, unsigned y)
{
  return x <=> y;
}
