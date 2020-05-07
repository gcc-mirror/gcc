// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94970
// { dg-do compile }

struct S94970
{
    string index() { return null; }
    ~this() { }
}

static m() { return S94970(); }

auto concat()
{
    return m.index ~ ' ';
}

auto newarray()
{
    return new int[][](m.index.length, 1);
}
