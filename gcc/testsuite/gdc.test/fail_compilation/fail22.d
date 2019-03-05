/*
TEST_OUTPUT:
---
fail_compilation/fail22.d(13): Error: no identifier for declarator `char`
---
*/

// infinite loop on DMD0.080

void main()
{
    char[] bug = "Crash";
    foreach(char ; bug) {}
}
