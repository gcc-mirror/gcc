module imports.test20023b;

auto threw()() @safe
{
    try
        throw new Exception("Hello");
    catch (Exception e)
        return e;
    assert(0);
}
