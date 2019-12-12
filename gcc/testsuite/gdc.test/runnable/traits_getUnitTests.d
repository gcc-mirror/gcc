// REQUIRED_ARGS: -unittest
// EXTRA_SOURCES: imports/traits_getUnitTests_import.d
module traits_getUnitTests;

import imports.traits_getUnitTests_import;

template Tuple (T...)
{
    alias Tuple = T;
}

int i;

unittest
{
    i++;
}

void test_getUnitTestsFromModule ()
{
   static assert(__traits(getUnitTests, mixin(__MODULE__)).length == 1);
}

struct SGetUnitTestsFromAggregate
{
    unittest {}
}

class CGetUnitTestsFromAggregate
{
    unittest {}
}

void test_getUnitTestsFromAggregate ()
{
    static assert(__traits(getUnitTests, SGetUnitTestsFromAggregate).length == 1);
    static assert(__traits(getUnitTests, CGetUnitTestsFromAggregate).length == 1);
}

void test_callUnitTestFunction ()
{
    __traits(getUnitTests, mixin(__MODULE__))[0]();
    assert(i == 2); // 2, because the standard unit test runner
                    // will call the unit test function as well
}

struct GetUnitTestsWithUDA
{
   @("asd") unittest {}
}

void test_getUnitTestsWithUDA ()
{
    alias tests = Tuple!(__traits(getUnitTests, GetUnitTestsWithUDA));
    static assert(tests.length == 1);
    static assert(__traits(getAttributes, tests[0]).length == 1);
}

void test_getUnitTestsFromImport ()
{
   static assert(__traits(getUnitTests, imports.traits_getUnitTests_import).length == 1);
   static assert(__traits(getUnitTests, mixin("imports.traits_getUnitTests_import")).length == 1);
}

// 11358
debug {  }
enum len11358 = __traits(getUnitTests, mixin(__MODULE__)).length;

void main ()
{
    test_getUnitTestsFromModule();
    test_getUnitTestsFromAggregate();
    test_callUnitTestFunction();
    test_getUnitTestsWithUDA();
    test_getUnitTestsFromImport();
}
