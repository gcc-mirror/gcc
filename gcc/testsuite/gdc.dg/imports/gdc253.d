module imports.gdc253;

interface I253a
{
}

interface I253b
{
    size_t printf(...);
    void flush();
}

class C253a : I253a , I253b
{
    size_t printf(...)
    {
        return 0;
    }

    void flush()
    {
    }
}
