// https://issues.dlang.org/show_bug.cgi?id=23439
// PERMUTE_ARGS: -lowmem
class C23439
{
    noreturn f23439;
}

__gshared ice23439 = new C23439();
