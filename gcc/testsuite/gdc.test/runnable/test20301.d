// https://github.com/dlang/dmd/issues/20301
void main()
{
}

string genline(string i)
{
    return "struct S" ~ i ~ " { S" ~ i ~ " *p; int x, y, z; } S" ~ i ~ "* foo" ~ i ~ "() { return null; }";
}

static foreach(i; 0..12288)
    mixin(genline(i.stringof));
