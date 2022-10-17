/*
REQUIRED_ARGS: -d
PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=16115
// https://github.com/dlang/dmd/pull/3979

int n;

struct Test
{
    enum tag = 42;
}

enum tagx = 42;

auto call()
{
    version (none) // works
    {
        n = Test.tag;
        return null;
    }
    else // assert error
    {
        //return n = tagx, null;
        return n = Test.tag;
        //return n = Test.tag;
    }
}

void main()
{
    call();

    assert(n == 42);
}
