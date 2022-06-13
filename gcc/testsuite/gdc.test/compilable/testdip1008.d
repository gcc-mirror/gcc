// PERMUTE_ARGS:
// REQUIRED_ARGS: -preview=dip1008

int bar()
{
    try
    {
	throw new Exception("message");
    }
    catch (Exception e)
    {
	return 7;
    }
}

void throwQualifiers() @safe @nogc pure
{
    throw new Exception("baz");
}

bool testThrowQualifiers()
{
    try
    {
        throwQualifiers();
    } catch (Exception e)
    {
        return true;
    }

    return false;
}

void foo()
{
    enum r = bar();
    static assert(r == 7);

    static assert(testThrowQualifiers());
}
