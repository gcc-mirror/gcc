/* TEST_OUTPUT:
---
fail_compilation/testsemi.d(102): Error: found `int` when expecting `;` following static assert
fail_compilation/testsemi.d(102): Error: variable name expected after type `x`, not `;`
fail_compilation/testsemi.d(109): Error: found `alias` when expecting `;` following alias reassignment
fail_compilation/testsemi.d(112): Error: found `}` when expecting `;` following invariant
fail_compilation/testsemi.d(117): Error: found `int` when expecting `;` following `alias Identifier this`
fail_compilation/testsemi.d(117): Error: variable name expected after type `x`, not `;`
fail_compilation/testsemi.d(123): Error: found `int` when expecting `;` following mixin
fail_compilation/testsemi.d(129): Error: found `int` when expecting `;` following `import` Expression
fail_compilation/testsemi.d(131): Error: `}` expected following members in `class` declaration
fail_compilation/testsemi.d(112):        class `C` starts here
---
 */

#line 100

static assert(1)
int x;

template map(alias F, Args...)
{
    alias A = AliasSeq!();
    static foreach (Arg; Args)
        A = AliasSeq!(A, F!Arg)
    alias staticMap = A;
}

class C { invariant(3) }

class D
{
    alias x this
    int x;
}

void test1()
{
    mixin("int x;")
    int y;
}

void test2()
{
    import(1)
    int z;
}
