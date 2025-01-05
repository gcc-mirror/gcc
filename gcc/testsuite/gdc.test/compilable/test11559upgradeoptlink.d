// REQUIRED_ARGS: -g

string gen()
{
    string m;
    foreach(i; 0..4096)
        m ~= "mixin(\"assert(0);\n\n\n\n\");\n";
    return m;
}

void main()
{
    mixin(gen());
}
