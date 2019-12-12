/*
TEST_OUTPUT:
---
fail_compilation/fail9413.d(45): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(32): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(33): Error: variable fail9413.foo.bar.y cannot modify parameter 'y' in contract
fail_compilation/fail9413.d(38): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(39): Error: variable fail9413.foo.bar.y cannot modify parameter 'y' in contract
fail_compilation/fail9413.d(40): Error: variable fail9413.foo.bar.s cannot modify result 's' in contract
fail_compilation/fail9413.d(50): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(73): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(74): Error: variable fail9413.foo.r cannot modify result 'r' in contract
fail_compilation/fail9413.d(58): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(59): Error: variable fail9413.foo.r cannot modify result 'r' in contract
fail_compilation/fail9413.d(60): Error: variable fail9413.foo.baz.y cannot modify parameter 'y' in contract
fail_compilation/fail9413.d(65): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(66): Error: variable fail9413.foo.r cannot modify result 'r' in contract
fail_compilation/fail9413.d(67): Error: variable fail9413.foo.baz.y cannot modify parameter 'y' in contract
fail_compilation/fail9413.d(68): Error: variable fail9413.foo.baz.s cannot modify result 's' in contract
fail_compilation/fail9413.d(79): Error: variable fail9413.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9413.d(80): Error: variable fail9413.foo.r cannot modify result 'r' in contract
---
*/

int foo(int x)
in
{
    int a;
    int bar(int y)
    in
    {
        x = 10; // err
        y = 10; // err
        a = 1;  // OK
    }
    out(s)
    {
        x = 10; // err
        y = 10; // err
        s = 10; // err
        a = 1;  // OK
    }
    body
    {
        x = 10; // err
        y = 1;  // OK
        a = 1;  // OK
        return 2;
    }
    x = 10; // err
}
out(r)
{
    int a;
    int baz(int y)
    in
    {
        x = 10; // err
        r = 10; // err
        y = 10; // err
        a = 1;  // OK
    }
    out(s)
    {
        x = 10; // err
        r = 10; // err
        y = 10; // err
        s = 10; // err
        a = 1;  // OK
    }
    body
    {
        x = 10; // err
        r = 10; // err
        y = 1;  // OK
        a = 1;  // OK
        return 2;
    }
    x = 10; // err
    r = 10; // err
}
body
{
    return 1;
}
