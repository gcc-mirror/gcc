// https://issues.dlang.org/show_bug.cgi?id=23431
// REQUIRED_ARGS: -lowmem
module object;

alias string  = immutable(char)[];
class Throwable { }
class Exception : Throwable
{
    this(string )
    {
    }
}

class Error { }

// Needed to lower `new Exception("ice")` to it.
T _d_newclassT(T)()
if (is(T == class))
{
    return null;
}

void test23431()
{
    int a;

    try
    {
        throw new Exception("test1");
        a++;
    }
    finally
    {
    }
}
