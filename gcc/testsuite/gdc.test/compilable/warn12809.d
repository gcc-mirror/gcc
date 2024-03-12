// REQUIRED_ARGS: -o- -w

/*
Warning removed in: https://github.com/dlang/dmd/pull/15568
---
fail_compilation/warn12809.d(25): Warning: statement is not reachable
fail_compilation/warn12809.d(33): Warning: statement is not reachable
---
*/

//void test_unrachable1()
//{
//    try assert(0);
//    finally
//    {
//        int x = 1;  // unreachable
//    }
//}

void test_unrachable2()
{
    try assert(0);
    finally {}

    int x = 1;      // unreachable
}

void test_unrachable3()
{
    try {}
    finally assert(0);

    int x = 1;      // unreachable
}

/********************************************/

/*

---
fail_compilation/warn12809.d(108): Warning: statement is not reachable
fail_compilation/warn12809.d(115): Warning: statement is not reachable
fail_compilation/warn12809.d(122): Warning: statement is not reachable
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

#line 100

alias noreturn = typeof(*null);

noreturn foo();

void test1(ref int i)
{
    foo();
    i = 3;
}

void test2()
{
    try foo();
    finally { }
    int x = 1;
}

void test3()
{
    try { }
    finally foo();
    int x = 1;
}

// https://issues.dlang.org/show_bug.cgi?id=14835
bool isEven(int i)()
{
    static if (i % 2)
        return true;
    return false;
}

enum x = isEven!0;

// https://issues.dlang.org/show_bug.cgi?id=10532
alias Seq(T...) = T;
void f()
{
    foreach (e; Seq!(10, 20))
    {
        if (e == 10)
            continue;

        // lots of code follows..
        auto x = 1;
    }
}
