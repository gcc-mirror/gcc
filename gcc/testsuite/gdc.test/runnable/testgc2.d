/*
PERMUTE_ARGS:
RUN_OUTPUT:
---
Success
---
*/

module testgc2;

import core.stdc.stdio;
import core.exception : OutOfMemoryError;

/*******************************************/

__gshared ulong counter;

void test1()
{
    try
    {
        long[] l = new long[ptrdiff_t.max];
        counter += l.capacity; // Make sure l is not optimized out.
        assert(0);
    }
    catch (OutOfMemoryError o)
    {
    }

    assert(counter == 0);

    try
    {
        byte[] b = new byte[size_t.max / 3];
        counter += b.capacity; // Make sure b is not optimized out.
        version (Windows)
            assert(0);
    }
    catch (OutOfMemoryError o)
    {
    }

    assert(counter >= 0);
}

/*******************************************/

void main()
{
    test1();

    printf("Success\n");
}
