// REQUIRED_ARGS: -debug

void main() nothrow
{
    debug
    {
        try
        {
            throw new Exception("2");
        }
        catch (Exception) {}
        catch (Throwable)
        {
            assert(0);
        }
    }
}
