module imports.linktypeinfo_file;

auto filter(alias pred, R)(R r)
{
    return FilterResult!(pred, R)(r);
}

struct FilterResult(alias pred, R)
{
    R r;
    bool empty() { return r.empty; }
    auto front() { return r.front; }
    void popFront()
    {
        while (!r.empty && pred(r.front))
            r.popFront();
    }
}

struct DirIterator
{
    int[] r;
    @property bool empty() { return r.length == 0; }
    @property auto front() { return r[0]; }
    void popFront() { r = r[1..$]; }

}

auto dirEntries(string path)
{
    bool f(int de) { return 1; }
    return filter!f(DirIterator());
}
