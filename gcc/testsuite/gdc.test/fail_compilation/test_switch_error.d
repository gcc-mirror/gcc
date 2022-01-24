/++
https://issues.dlang.org/show_bug.cgi?id=22514
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(13): Error: undefined identifier `doesNotExist`
fail_compilation/test_switch_error.d(16): Error: undefined identifier `alsoDoesNotExits`
fail_compilation/test_switch_error.d(19): Error: duplicate `case 2` in `switch` statement
---
++/

void test1()
{
    switch (doesNotExist)
    {
        case 1:
            alsoDoesNotExits();
            break;
        case 2: break;
        case 2: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(105): Error: undefined identifier `doesNotExist`
---
++/
#line 100

enum foo = 1;

void test2()
{
    switch (doesNotExist)
    {
        case foo: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(206): Error: undefined identifier `a`
fail_compilation/test_switch_error.d(207): Error: undefined identifier `b`
---
++/
#line 200

void test3()
{

    switch (1)
    {
        case a: break;
        case b: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(303): Error: undefined identifier `doesNotExits`
---
++/
#line 300

void test4()
{
    auto foo = doesNotExits();
    switch (1)
    {
        case foo: break;
        case foo: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(405): Error: `case` variables have to be `const` or `immutable`
fail_compilation/test_switch_error.d(412): Error: `case` variables not allowed in `final switch` statements
---
++/
#line 400

void test5(int i)
{
    switch (i)
    {
        case i: break;
        default: break;
    }

    const int j = i;
    final switch (i)
    {
        case j: break;

    }
}
