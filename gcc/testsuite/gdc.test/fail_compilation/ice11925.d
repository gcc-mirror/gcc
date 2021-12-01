/* TEST_OUTPUT:
---
fail_compilation/ice11925.d(23): Error: cannot `goto` into `try` block
fail_compilation/ice11925.d(31): Error: cannot `goto` into `try` block
---
*/

void test11925a()
{
    try
    {
        try
        {
            L1: {}
        }
        finally
        {
        }
    }
    finally
    {
    }
    goto L1;
}

void test11925b()
{
    switch (1)
    {
        case 1:
            goto L1;
            break;

        default:
            break;
    }

    try
    {
        L1: { }
    }
    finally
    {
    }
}
