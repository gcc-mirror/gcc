/*
PERMUTE_ARGS: -O -inline
RUN_OUTPUT:
---
getAndPrintS
---
*/

import core.stdc.stdio;
import core.exception : AssertError;

/*****************************************/

// noreturn is inferred for functions that always throw
// The code must not strip the destructor when calling a noreturn function

struct WithDtor
{
    __gshared int destroyed;

    int num;

    int acceptNoreturn(int a, int b, int c)
    {
        puts("unreachable");
        return num + a + b + c;
    }

    ~this()
    {
        destroyed += num;
    }
}

noreturn doesThrow()
{
    WithDtor wd = WithDtor(1);
    throw new Exception("");
}

noreturn callDoesThrow()
{
    WithDtor wd = WithDtor(2);
    doesThrow();
}


void testDtors()
{
    try
    {
        callDoesThrow();
        assert(0);
    } catch (Exception e) {}

    assert(WithDtor.destroyed == 3);
}

/*****************************************************************************/

/// Verifies that `func` throws a `Throwable` with `message` at `line`
void testAssertFailure(size_t expLine, string expMsg, void function() func, size_t callLine = __LINE__)
{
    void enforce(bool check, string error)
    {
        if (!check)
            throw new AssertError(error, __FILE__, callLine);
    }

    bool caught;
    try
    {
        func();
    }
    catch (Throwable t)
    {
        // Save members because t might be overwritten by an Assertion failure below
        string actFile = t.file;
        size_t actLine = t.line;
        string actMsg = t.msg;
        caught = true;

        scope (failure)
        {
            printf("\nfile = \"%.*s\"\nline = %zu\nmsg = \"%.*s\"\n\n",
                cast(int) actFile.length, actFile.ptr,
                actLine,
                cast(int) actMsg.length, actMsg.ptr
            );
            fflush(stdout);
        }

        enforce(actFile == __FILE__, "Wrong file");
        enforce(actLine == expLine, "Wrong line");
        enforce(actMsg == expMsg, "Wrong message");
    }

    enforce(caught, "No Throwable was thrown!");
}

void testAccess()
{
    enum msg = "Accessed expression of type `noreturn`";

    // FIXME: Another assertion failure in the backend trying to generate noreturn.sizeof = 0 byte assignment
    version (FIXME)
    testAssertFailure(__LINE__ + 3, msg, function noreturn()
    {
        noreturn a;
        noreturn b = a;
    });

    if (false) // read does not assert!
    testAssertFailure(__LINE__ + 3, msg, function noreturn()
    {
        noreturn a;
        int b = a;
        assert(false, "Unreachable!"); // Statement above not detected as noreturn
    });

    testAssertFailure(__LINE__ + 2, msg, function noreturn()
    {
        cast(noreturn) 1;
    });

    version (FIXME)
    testAssertFailure(__LINE__ + 3, msg, function noreturn()
    {
        noreturn a;
        noreturn b = cast(noreturn) 1;
    });

    if (false) // Read does not assert
    testAssertFailure(__LINE__ + 3, msg, function noreturn()
    {
        noreturn a;
        return a;
    });

    if (false) // Read does not assert
    testAssertFailure(__LINE__ + 4, msg, function noreturn()
    {
        static void foo(noreturn) {}
        noreturn a;
        foo(a);
        assert(false, "Unreachable!"); // Ditto
    });
}

/*****************************************/

void testFuncCall()
{
    enum msg = "Called abort()";
    enum line = __LINE__ + 1;
    static noreturn abort() { assert(0, msg); }

    // Canaries to check for side effects
    __gshared int countLeft, countRight;

    scope (failure) printf("countLeft = %d\ncountRight = %d\n", countLeft, countRight);


    // D function arguments are evaluated left to right
    testAssertFailure(line, msg, function()
    {
        static void acceptNoreturnD(int, int, int) { puts("unreachable"); }

        acceptNoreturnD(countLeft++, abort(), countRight++);
    });

    assert(countLeft == 1);
    assert(countRight == 0);

//     // C function arguments are still evaluated left to right
//     // Despite them being push in reverse order
    testAssertFailure(line, msg, function()
    {
        static extern(C) void acceptNoreturnC(int, int, int) { puts("unreachable"); }

        acceptNoreturnC(countLeft++, abort(), countRight++);

        assert(false);
    });

    assert(countLeft == 2);
    assert(countRight == 0);

    WithDtor.destroyed = 0;

    testAssertFailure(__LINE__ + 2, "Error", function()
    {
        static WithDtor getS() { assert(false, "Error"); }

        getS().acceptNoreturn(countLeft++, abort(), countRight++);
    });

    assert(countLeft == 2); // No changes
    assert(countRight == 0);
    assert(WithDtor.destroyed == 0); // No temporary to destruct

    testAssertFailure(line, msg, function()
    {
        static WithDtor getAndPrintS() { puts("getAndPrintS"); return WithDtor(1); }

        getAndPrintS().acceptNoreturn(countLeft++, abort(), countRight++);
    });

    assert(countLeft == 3);
    assert(countRight == 0);
    assert(WithDtor.destroyed == 1);
}

// https://issues.dlang.org/show_bug.cgi?id=24701
void testCast()
{
    noreturn foo;
    try
        auto a = cast(int)foo;
    catch (Throwable e)
    {
        assert(e.msg == "Accessed expression of type `noreturn`");
        return;
    }

    assert(0);
}

int main()
{
    testDtors();
    testAccess();
    testFuncCall();
    testCast();
    return 0;
}
