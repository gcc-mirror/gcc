shared static this() {
    throw new Exception("module constructor");
}

shared static ~this() {
    import core.stdc.stdio;
    puts("module destructor");
}

void main() {}
