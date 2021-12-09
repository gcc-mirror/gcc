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


void foo()
{
    enum r = bar();
    static assert(r == 7);
}
