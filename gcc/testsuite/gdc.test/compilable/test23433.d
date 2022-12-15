// https://issues.dlang.org/show_bug.cgi?id=23433
module object;

class Throwable { }
class Exception : Throwable { this(immutable(char)[]) { } }

void test23433()
{
    try
    {
        throw new Exception("ice");
    }
    finally
    {
    }
}
