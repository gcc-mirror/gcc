/*
REQUIRED_ARGS: -w -o-

TEST_OUTPUT:
---
fail_compilation/noreturn2.d(18): Error: expected return type of `noreturn`, not `void`
---

https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1034.md
*/

alias noreturn = typeof(*null);

void doStuff();

noreturn returnVoid()
{
    return doStuff();
}


/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(37): Error: expected return type of `int`, not `string`:
fail_compilation/noreturn2.d(35):        Return type of `int` inferred here.
---
+/

auto missmatch(int i)
{
    if (i < 0)
        return assert(false);
    if (i == 0)
        return i;
    if (i > 0)
        return "";
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(50): Error: function `noreturn2.returns` is typed as `NR` but does return
fail_compilation/noreturn2.d(50):        `noreturn` functions must either throw, abort or loop indefinitely
---
+/

enum NR : noreturn;

NR returns()
{
    // Fallthrough despite noreturn
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(64): Error: cannot implicitly convert expression `1` of type `int` to `noreturn`
---
+/

noreturn returnsValue()
{
    return 1;
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(75): Error: expected return type of `int`, not `void`
---
+/
int returnVoid2()
{
    return doStuff();
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(89): Error: mismatched function return type inference of `void` and `int`
---
+/
auto returnVoid3(int i)
{
    if (i > 0)
        return i;
    else
        return doStuff();
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(104): Error: `object.Exception` is thrown but not caught
fail_compilation/noreturn2.d(100): Error: function `noreturn2.doesNestedThrow` may throw but is marked as `nothrow`
---
+/

int doesNestedThrow(int i) nothrow
{
    // Weird formatting is intended to check the loc
    return i ? i++ :
            throw
            new
            Exception("")
    ;
}

int doesNestedThrowThrowable(int i) nothrow
{
    return i ? i++ : throw new Error("");
}

/+
TEST_OUTPUT:
---
fail_compilation/noreturn2.d(130): Error: cannot create instance of interface `I`
fail_compilation/noreturn2.d(133): Error: can only throw class objects derived from `Throwable`, not type `int[]`
fail_compilation/noreturn2.d(138): Error: undefined identifier `UnkownException`
---
+/

int throwInvalid(int i) nothrow
{
    static interface I {}
    // Weird formatting is intended to check the loc
    return
            throw
            new
            I()
        ?
            throw
            new
            int[4]
        :
            throw
            new
            UnkownException("")
    ;
}
