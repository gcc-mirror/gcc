void foo(T)(ref T t)
{
}

struct S
{
    int impure() {assert(0);}
    alias impure this;
}

void main() pure
{
    S s;
    foo(s);
    s.foo(); // triggering alias this violates purity, but ufcs matches
}
