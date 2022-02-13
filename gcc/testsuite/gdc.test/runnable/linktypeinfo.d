// EXTRA_SOURCES: imports/linktypeinfo_file.d
// PERMUTE_ARGS: -g -inline -unittest -debug
// COMPILE_SEPARATELY:

import imports.linktypeinfo_file;

struct Only(T)
{
    private T _val;
}

auto only(V)(V v)
{
    return Only!V(v);
}

static struct Chain(R...)
{
    R source;
}

auto chain(R...)(R rs)
{
    return Chain!R(rs);
}

void main()
{
    string docRoot;

    const r = dirEntries(docRoot);
    typeof(r)[] a;
    a.length = 0;   // require TypeInfo for const(FilterResult!(DirIterator))
}
