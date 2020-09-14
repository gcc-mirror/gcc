// https://bugzilla.gdcproject.org/show_bug.cgi?id=36
// { dg-additional-sources "imports/gdc36.d" }
// { dg-options "-I $srcdir/gdc.dg" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

module gdc36;

import imports.gdc36;

/**
 * Here getChar is a function in a template where template.isnested == false
 * but getChar still is a nested function and needs to get a static chain
 * containing test36a.
 */
void test36a()(char val)
{
    void error()
    {
    }

    void getChar()()
    {
        error();
    }

    void parseString()
    {
        getChar();
    }
}

/**
 * Similar as test36a, but a little more complicated:
 * Here getChar is nested in a struct template which is nested in a function.
 * getChar's static chain still needs to contain test36b.
 */
void test36b()(char val)
{
    void error()
    {
    }

    struct S(T)
    {
        void getChar()
        {
            error();
        }
    }


    void parseString()
    {
        S!(int)().getChar();
    }
}

/**
 * If g had accessed a, the frontend would have generated a closure.
 *
 * As we do not access it, there's no closure. We have to be careful
 * not to set a static chain for g containing test36c_1 though,
 * as g can be called from outside (here from test1c). In the end
 * we have to treat this as if everything in test36c_1 was declared
 * at module scope.
 */
auto test36c_1()
{
    int a;
    void c() {}
    class Result
    {
        int b;
        void g() { c(); /*a = 42;*/ }
    }

    return new Result();
}

void test36c()
{
    test36c_1().g();
}

/**
 * empty is a (private) function which is nested in lightPostprocess.
 * At the same time it's a template instance, so it has to be declared as
 * weak or otherwise one-only. imports/gdc36.d creates another instance
 * of Regex!char to verify that.
 */
struct Parser(R)
{
    @property program()
    {
        return Regex!char();
    }
}

struct Regex(Char)
{
    @trusted lightPostprocess()
    {
        struct FixedStack(T)
        {
            @property empty() { return false; }
        }
        auto counterRange = FixedStack!uint();
    }
}

void test36d()
{
    auto parser = Parser!(char[])();
    imports.gdc36.test36d_1;
}

void main()
{
  test36a('n');
  test36b('n');
  test36c();
  test36d();
}

