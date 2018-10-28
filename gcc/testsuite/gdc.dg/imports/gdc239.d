import std.path : buildNormalizedPath;

class C239a
{
    auto bar()
    {
        auto path = buildNormalizedPath("/", "foo");
    }
}
