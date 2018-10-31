// REQUIRED_ARGS: -o- -w

/*
TEST_OUTPUT:
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
