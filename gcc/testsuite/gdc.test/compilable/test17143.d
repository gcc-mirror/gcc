import std.typecons : tuple;
enum foo = tuple(1, 2).expand;
pragma(msg, typeof(foo).stringof);
pragma(msg, foo.stringof);
