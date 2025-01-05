// PERMUTE_ARGS:

// COMDAT folding increases runtime by > 80x
// REQUIRED_ARGS(windows): -L/OPT:NOICF

// Generate \sum_{i=0}^{14} 2^i = 32767 template instantiations
// (each with 3 sections) to use more than 64Ki sections in total.

size_t foo(size_t i, size_t mask)()
{
    static if (i == 14)
        return mask;
    else
        return foo!(i + 1, mask) + foo!(i + 1, mask | (1UL << i));
}

void main()
{
    assert(foo!(0, 0) != 0);
}
