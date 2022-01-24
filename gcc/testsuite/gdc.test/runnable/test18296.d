// REQUIRED_ARGS: -cov
// PERMUTE_ARGS: -fPIC
alias AliasSeq(Args...) = Args;

struct Duration
{
    this(long hnsecs)
    {
        _hnsecs = hnsecs;
    }


    long _hnsecs;
}

void main()
{
    foreach(U; AliasSeq!(Duration, const Duration))
    {
        const Duration t = 42;
        U u = t;
        assert(t._hnsecs == u._hnsecs);
    }
}
