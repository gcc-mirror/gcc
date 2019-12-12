module imports.fail17646;

struct TestData
{
}

const(TestData)[] allTestData(MOD_STRINGS...)()
{
    foreach (i; MOD_STRINGS)
}
