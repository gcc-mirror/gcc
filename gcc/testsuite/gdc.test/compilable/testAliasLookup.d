// REQUIRED_ARGS: -preview=fixAliasThis

// https://issues.dlang.org/show_bug.cgi?id=16086
struct A
{
    void tail() {}
}

struct S16086
{
    struct Inner2
    {
        Inner a;
        alias a this;
    }

    struct Inner
    {
        int unique_identifier_name;
        int tail = 2;
    }

    Inner2 inner;
    alias inner this;

    auto works()
    {
        return unique_identifier_name;
    }

    auto fails()
    {
        int a = tail;
        return tail; // Line 22
        // The workaround:  return this.tail;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=16082
struct S16082
{
    struct Inner
    {
        int any_name_but_modulename;
        int aliasthis = 5;
    }

    Inner inner;
    alias inner this;

    auto works()
    {
        return any_name_but_modulename;
    }
    auto fails()
    {
        return aliasthis;  // Line 20
    }
}

