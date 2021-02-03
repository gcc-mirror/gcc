import std.typecons : tuple;
enum foo = tuple(1, 2).expand;
static assert(typeof(foo).stringof == "(int, int)");
static assert(foo.stringof == "tuple(1, 2)");
