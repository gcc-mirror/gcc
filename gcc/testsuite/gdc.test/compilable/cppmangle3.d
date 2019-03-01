module cppmangle3;


extern(C++, "true")
{
}

extern(C++, "__traits")
{
}

extern(C++, "foo")
{
}

int foo; // no name clashing with above namespace

extern(C++, "std", "chrono")
{
    void func();
}

version(Windows) static assert(func.mangleof == "?func@chrono@std@@YAXXZ");
else             static assert(func.mangleof == "_ZNSt6chrono4funcEv");

struct Foo
{
    extern(C++, "namespace")
    {
        static void bar();
    }
}

alias Alias(alias a) = a;
alias Alias(T) = T;

static assert(is(Alias!(__traits(parent, bar)) == Foo));
