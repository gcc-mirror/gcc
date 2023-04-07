// https://issues.dlang.org/show_bug.cgi?id=23578
struct S(alias a)
{
}

static if (is(S!int == S!av, alias av))
    static assert(is(av == int));
else
    static assert(false);
