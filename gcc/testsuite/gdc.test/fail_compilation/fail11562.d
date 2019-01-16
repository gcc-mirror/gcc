/*
REQUIRED_ARGS: -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/fail11562.d(16): Error: cannot goto in or out of `finally` block
fail_compilation/fail11562.d(37): Error: cannot goto in or out of `finally` block
fail_compilation/fail11562.d(49): Error: cannot goto in or out of `finally` block
fail_compilation/fail11562.d(64): Error: cannot goto in or out of `finally` block
---
*/

// Goto into finally block (forwards)
int w(bool b)
{
    if (b) goto label;
    try
    {
    }
    finally
    {
    label: {}
    }
    return 1;
}

// // Goto into finally block (backwards)
int x(bool b)
{
    try
    {
    }
    finally
    {
    label: {}
    }
    if (b) goto label;
    return 1;
}

// Goto out of finally block (forwards)
int y(bool b)
{
    try
    {
    }
    finally
    {
    if (b) goto label;
    }
    label: {}
    return 1;
}

// // Goto out of finally block (backwards)
int z(bool b)
{
    label: {}
    try
    {
    }
    finally
    {
    if (b) goto label;
    }
    return 1;
}
