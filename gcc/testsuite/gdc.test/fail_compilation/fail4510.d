// https://issues.dlang.org/show_bug.cgi?id=4510
/*
TEST_OUTPUT:
---
fail_compilation/fail4510.d(12): Error: argument type mismatch, `float` to `ref double`
---
*/

void main()
{
    float[] arr = [1.0, 2.5, 4.0];
    foreach (ref double elem; arr) {
        //elem /= 2;
    }
}
