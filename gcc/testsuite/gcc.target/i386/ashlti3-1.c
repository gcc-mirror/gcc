/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */
__int128 foo(__int128 x, unsigned long long b) {
    return ((__int128)b << 72) + x;
}
/* { dg-final { scan-assembler-not "\tmovl\[ \\t\]+\\\$0," } } */
