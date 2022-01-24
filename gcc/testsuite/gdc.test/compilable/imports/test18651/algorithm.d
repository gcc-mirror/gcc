struct SortedRange(Range)
{
    Range _input;
}

auto sort(Range)(Range)
{
    return SortedRange!Range();
}

auto uniq(Range)(Range)
{
    return SortedRange!Range();
}
