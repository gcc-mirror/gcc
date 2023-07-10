// https://issues.dlang.org/show_bug.cgi?id=24017

// REQUIRED_ARGS: -debug

void writeln(string) {}

void main() nothrow
{
    debug writeln("Hello");
    debug "Hello".writeln;
}
