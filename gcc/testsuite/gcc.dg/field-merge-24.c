/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR tree-optimization/118572 */
/* Check that signed compares with constants that seem signed in the other
   compare operand's width get treated as unsigned if its upper bits are masked
   out.  */

__attribute__((noipa))
int test(signed char c)
{
    return (((0x80 & (c&0xff)) != 0) && ((0xc0 & (c&0xff)) == 0x80));
}

__attribute__((noipa))
int test2(signed char c)
{
    return (((-128 & (c&-1)) != 0) && ((-64 & (c&-1)) == -128));
}

__attribute__((noipa))
int test3(signed char c)
{
    return (((0x80 & (c&-1)) != 0) && ((0x1248c0 & (c&-1)) == 0x124880));
}

__attribute__((noipa))
int test4(signed char c)
{
    return (((0x400 & (c&-1)) == 0) && ((0x40 & (c&-1)) == 0x40));
}

int main() {
  if (test(0x80) == 0 || test2(-128) == 0 || test3(-128) == 0 || test4(64) == 0)
        __builtin_abort();
}
