/*
TEST_OUTPUT:
---
fail_compilation/fail22.d(13): Error: variable name expected after type `char`, not `;`
---
*/

// infinite loop on DMD0.080

void main()
{
    char[] bug = "Crash";
    foreach(char ; bug) {}
}
