/*
TEST_OUTPUT:
---
fail_compilation/fix20867.d(14): Error: cannot use `final switch` on enum `E` while it is being defined
---
*/

// Test case from Issue #20867
enum E
{
    a = 3,
    b = () {
        E e;
        final switch (e)  // This should error out instead of segfaulting
        {
            case E.a: break;
        }
        return 4;
    } ()
}
