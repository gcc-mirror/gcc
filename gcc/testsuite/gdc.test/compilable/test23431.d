// https://issues.dlang.org/show_bug.cgi?id=23431
// REQUIRED_ARGS: -lowmem
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
