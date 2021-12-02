// https://issues.dlang.org/show_bug.cgi?id=9150
// Mismatching static array length should be detected in foreach
/*
TEST_OUTPUT:
---
fail_compilation/test9150.d(14): Error: mismatched array lengths, 5 and 3
---
*/

void main()
{
    int[3][2] matrix = [ [1,11,111], [2,22,222] ];

    foreach (int[5] row; matrix) //if int[3], there is no error.
    {
        foreach (x; row)
        {}//write(x, "  ");

        //writeln();
    }
}
