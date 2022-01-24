// EXECUTE_ARGS: foo bar doo
// PERMUTE_ARGS:
import core.stdc.stdio;
import std.conv;
import core.runtime;

void main(string[] args)
{
    string[] dArgs = Runtime.args;
    CArgs cArgs = Runtime.cArgs;

    assert(dArgs.length && cArgs.argc);  // ensure we've passed some args
    assert(dArgs.length == cArgs.argc);

    assert(dArgs[1] == to!string(cArgs.argv[1]));
    assert(args[1] == to!string(cArgs.argv[1]));
}
