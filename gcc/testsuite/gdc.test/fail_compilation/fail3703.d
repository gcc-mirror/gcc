// https://issues.dlang.org/show_bug.cgi?id=3703
// static array assignment
/*
TEST_OUTPUT:
---
fail_compilation/fail3703.d(18): Error: mismatched array lengths, 2 and 1
fail_compilation/fail3703.d(20): Error: mismatched array lengths, 2 and 1
fail_compilation/fail3703.d(22): Error: mismatched array lengths, 3 and 2
fail_compilation/fail3703.d(23): Error: mismatched array lengths, 2 and 3
fail_compilation/fail3703.d(25): Error: mismatched array lengths, 3 and 2
fail_compilation/fail3703.d(26): Error: mismatched array lengths, 2 and 3
---
*/

void main()
{
    int[1] a = [1];
    int[2] b = a;   // should make compile error

    b = a;  // should make compile error

    int[3] sa3 = [1,2][];
    int[2] sa2 = sa3[][];

    sa3 = [1,2][];
    sa2 = sa3[][];
}
