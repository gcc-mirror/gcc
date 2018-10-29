// REQUIRED_ARGS: -g

// If this is failing, you need optlink 8.00.14 or higher

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
