// https://issues.dlang.org/show_bug.cgi?id=21674
// REQUIRED_ARGS: -de

struct Module
{
    CachedString data;
}

struct CachedString
{
    private size_t len;

    this (string data) { this.len = data.length; }
    public string str () const { return null; }
    public void str (string value) { this.len = value.length; }

    alias str this;
}

void test21674a()
{
    Module m;
    m.data = "Hello World";
}

//////////////////////////////////////////

struct StaticGetter(T)
{
    private static T _impl;
    static ref T value() { return _impl; }
    alias value this;
}

struct StaticWrapper
{
    StaticGetter!int get;
    alias get this;
}

void test21674b()
{
    StaticGetter!float sg;
    sg = 4.2;

    StaticWrapper sw;
    sw = 42;
}

//////////////////////////////////////////

EntryType arr;
auto getPtr() { return &arr; }

struct EntryType
{
    bool _state;
    alias _state this;
}

struct S19441
{
    @property auto ref entry() { return *getPtr(); }
    alias entry this;
}

void test19441()
{
    S19441 s19441;
    s19441 = true;
}
