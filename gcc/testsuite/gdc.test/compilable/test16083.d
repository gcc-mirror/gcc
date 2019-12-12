template Alias(Stuff...)
{
    alias Alias = Stuff;
}

enum A { a = 0 }
enum B { b = 0 }

enum C { c = "abc" }
enum D { d = "abc" }

static assert(is(typeof(Alias!(A.a)[0]) == A));
static assert(is(typeof(Alias!(B.b)[0]) == B));
static assert(is(typeof(Alias!(C.c)[0]) == C));
static assert(is(typeof(Alias!(D.d)[0]) == D));
