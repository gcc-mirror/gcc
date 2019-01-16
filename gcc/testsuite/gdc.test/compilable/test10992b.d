// PERMUTE_ARGS:
// REQUIRED_ARGS: -unittest

version(none)
{}
else
{
    unittest { }
    unittest { }
    unittest { }
}

void main()
{
    static assert(__traits(getUnitTests, mixin(__MODULE__)).length == 3);
}
