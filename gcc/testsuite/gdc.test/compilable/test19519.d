struct S { char[1] e = void; }

void init(ref char[1] e)
{
    e[0 .. 1] = "A";
}

int foo()
{
    auto s = S();
    init(s.e);
    return __ctfe;
}

static assert(foo() == 1);
