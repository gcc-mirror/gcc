// REQUIRED_ARGS: -unittest
module issue18097;

unittest // this first unittest is needed to trigger the bug
{
}

unittest // second unittest
{
    auto a = &mixin(__traits(identifier, __traits(parent, {  })));
    auto b = &__traits(parent, {  });
}
