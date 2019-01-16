/*
TEST_OUTPUT:
---
fail_compilation/diag9831.d(12): Error: function diag9831.main.__lambda1 cannot access frame of function D main
---
*/

void main()
{
    immutable int c;
    int function(int x) func;
    func = x => c;
}
