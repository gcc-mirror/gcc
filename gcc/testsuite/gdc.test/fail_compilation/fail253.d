/*
TEST_OUTPUT:
---
fail_compilation/fail253.d(13): Error: variable fail253.main.x inout variables can only be declared inside inout functions
fail_compilation/fail253.d(16): Error: cannot modify inout expression x
---
*/

void main()
{
    foreach (i; 0 .. 2)
    {
        foreach (inout char x; "hola")
        {
            //printf("%c", x);
            x = '?';
        }
    }
}
