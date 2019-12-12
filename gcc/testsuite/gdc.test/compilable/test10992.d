// PERMUTE_ARGS:
// REQUIRED_ARGS: -unittest

unittest { }
unittest { }
unittest { }

void main()
{
    static assert(__traits(getUnitTests, mixin(__MODULE__)).length == 3);
}
