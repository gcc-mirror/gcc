// https://bugzilla.gdcproject.org/show_bug.cgi?id=71
// { dg-do compile }

struct CanonicalHuffman
{
    int[] table;

    void print()
    {
        table.sort!(a => a);
    }
}

struct SortedRange(alias pred )
{
    auto trisect() { }
}

auto assumeSorted(alias pred , R)(R )
{
    return SortedRange!pred();
}

SortedRange!(less) sort(alias less, Range)(Range r)
{
    return assumeSorted!less(r);
}
