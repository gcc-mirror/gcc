/*
TEST_OUTPUT:
---
fail_compilation/fail12604.d(14): Error: mismatched array lengths, 1 and 3
fail_compilation/fail12604.d(15): Error: mismatched array lengths, 1 and 3
fail_compilation/fail12604.d(17): Error: mismatched array lengths, 1 and 3
fail_compilation/fail12604.d(18): Error: mismatched array lengths, 1 and 3
fail_compilation/fail12604.d(20): Error: cannot implicitly convert expression `[65536]` of type `int[]` to `short[]`
fail_compilation/fail12604.d(21): Error: cannot implicitly convert expression `[65536, 2, 3]` of type `int[]` to `short[]`
---
*/
void main()
{
      int[1] a1 = [1,2,3];
    short[1] a2 = [1,2,3];

      int[1] b1; b1 = [1,2,3];
    short[1] b2; b2 = [1,2,3];

    short[1] c = [65536];
    short[1] d = [65536,2,3];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12604.d(39): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(40): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(41): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(42): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(43): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(44): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(45): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(46): Error: mismatched array lengths, 2 and 3
---
*/
void test12606a()   // AssignExp::semantic
{
      uint[2] a1 = [1, 2, 3][];
    ushort[2] a2 = [1, 2, 3][];
      uint[2] a3 = [1, 2, 3][0 .. 3];
    ushort[2] a4 = [1, 2, 3][0 .. 3];
    a1 = [1, 2, 3][];
    a2 = [1, 2, 3][];
    a3 = [1, 2, 3][0 .. 3];
    a4 = [1, 2, 3][0 .. 3];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12604.d(60): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(61): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(62): Error: mismatched array lengths, 2 and 3
fail_compilation/fail12604.d(63): Error: mismatched array lengths, 2 and 3
---
*/
void test12606b()   // ExpInitializer::semantic
{
    static   uint[2] a1 = [1, 2, 3][];
    static   uint[2] a2 = [1, 2, 3][0 .. 3];
    static ushort[2] a3 = [1, 2, 3][];
    static ushort[2] a4 = [1, 2, 3][0 .. 3];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12604.d(77): Error: mismatched array lengths 4 and 3 for assignment `sa1[0..4] = [1, 2, 3]`
fail_compilation/fail12604.d(78): Error: mismatched array lengths 4 and 3 for assignment `sa1[0..4] = sa2`
---
*/
void testc()
{
    int[4] sa1;
    int[3] sa2;
    sa1[0..4] = [1,2,3];
    sa1[0..4] = sa2;
}
