template Alias(alias a)
{
    alias Alias = a;
}

void main()
{
    auto scale = 4;
    alias edentity = a => a * scale;
    static assert(__traits(isSame, Alias!edentity, edentity)); // fails in dmd-nightly
}
