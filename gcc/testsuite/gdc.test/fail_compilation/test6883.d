/*
TEST_OUTPUT:
---
fail_compilation/test6883.d(15): Error: array index 5 is out of bounds `x[0 .. 5]`
fail_compilation/test6883.d(17): Error: array index 7 is out of bounds `x[0 .. 5]`
fail_compilation/test6883.d(21): Error: array index 5 is out of bounds `x[0 .. 5]`
fail_compilation/test6883.d(23): Error: array index 7 is out of bounds `x[0 .. 5]`
---
*/

void main()
{
    {
        int[5] x;
        x[x.length] = 1;
        enum size_t n = 2;
        x[x.length + n] = 2;
    }
    {
        int[5] x;
        x[$] = 1;
        enum size_t n = 2;
        x[$ + n] = 2;
    }
}
