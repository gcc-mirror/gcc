static foreach(s; ["666", "777", "888"])
{
    mixin(genCtor(s));
}

int i;

string genCtor(string a)
{
    return "static this() { i += " ~ a ~ "; }";
}

void main()
{
    assert(i == 0 + 666 + 777 + 888);
}
