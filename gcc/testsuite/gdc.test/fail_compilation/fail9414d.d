/*
TEST_OUTPUT:
---
fail_compilation/fail9414d.d(47): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(34): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(35): Error: variable fail9414d.C.foo.bar.y cannot modify parameter 'y' in contract
fail_compilation/fail9414d.d(40): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(41): Error: variable fail9414d.C.foo.bar.y cannot modify parameter 'y' in contract
fail_compilation/fail9414d.d(42): Error: variable fail9414d.C.foo.bar.s cannot modify result 's' in contract
fail_compilation/fail9414d.d(52): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(75): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(76): Error: variable fail9414d.C.foo.r cannot modify result 'r' in contract
fail_compilation/fail9414d.d(60): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(61): Error: variable fail9414d.C.foo.r cannot modify result 'r' in contract
fail_compilation/fail9414d.d(62): Error: variable fail9414d.C.foo.baz.y cannot modify parameter 'y' in contract
fail_compilation/fail9414d.d(67): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(68): Error: variable fail9414d.C.foo.r cannot modify result 'r' in contract
fail_compilation/fail9414d.d(69): Error: variable fail9414d.C.foo.baz.y cannot modify parameter 'y' in contract
fail_compilation/fail9414d.d(70): Error: variable fail9414d.C.foo.baz.s cannot modify result 's' in contract
fail_compilation/fail9414d.d(81): Error: variable fail9414d.C.foo.x cannot modify parameter 'x' in contract
fail_compilation/fail9414d.d(82): Error: variable fail9414d.C.foo.r cannot modify result 'r' in contract
---
*/

class C
{
    static int foo(int x)
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
}
