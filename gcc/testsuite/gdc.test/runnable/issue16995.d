// REQUIRED_ARGS: -unittest
// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/module_with_tests.d imports/another_module_with_tests.d

import imports.module_with_tests;
import imports.another_module_with_tests;
import core.exception: AssertError;

shared static this()
{
    import core.runtime: Runtime, UnitTestResult;
    Runtime.extendedModuleUnitTester = () => UnitTestResult.pass;
}

void main()
{
    foreach(i, ut; __traits(getUnitTests, imports.module_with_tests))
    {
        try
        {
            ut();
            assert(i == 0, "2nd unittest should fail");
        }
        catch(AssertError e)
        {
            assert(i == 1, "Only 2nd unittest should fail");
        }
    }

    foreach(i, ut; __traits(getUnitTests, imports.another_module_with_tests))
    {
        try
        {
            ut();
            assert(i == 0 || i == 1, "3rd unittest should fail");
        }
        catch(AssertError e)
        {
            assert(i == 2, "Only 3rd unittest should fail");
        }
    }
}
