auto inferNothrow()
in
{
}
out
{
}
do
{
    return 1;
}

auto dontInferNothrowIn()
in
{
    throw new Exception(null);
}
do
{
    return 1;
}

auto dontInferNothrowOut()
out
{
    throw new Exception(null);
}
do
{
    return 1;
}

enum isNothrow(Attr...) = (Attr.length >= 1)
    && (Attr[0] == "nothrow" || isNothrow!(Attr[1 .. $]));

static assert(isNothrow!(__traits(getFunctionAttributes, inferNothrow)));
static assert(!isNothrow!(__traits(getFunctionAttributes, dontInferNothrowIn)));
static assert(!isNothrow!(__traits(getFunctionAttributes, dontInferNothrowOut)));
