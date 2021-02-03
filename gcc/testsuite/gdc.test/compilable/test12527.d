// https://issues.dlang.org/show_bug.cgi?id=12527

@system:
    alias Fun = void function() @safe;
    static assert(Fun.stringof == "void function() @safe");
    alias Del = void delegate() @safe;
    static assert(Del.stringof == "void delegate() @safe");

