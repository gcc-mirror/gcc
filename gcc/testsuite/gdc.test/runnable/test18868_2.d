mixin(genCtor("666")); mixin(genCtor("777"));

int i;

string genCtor(string a)
{
    return "static this() { i += " ~ a ~ "; }";
}

void main()
{
    assert(i == 0 + 666 + 777);
}
