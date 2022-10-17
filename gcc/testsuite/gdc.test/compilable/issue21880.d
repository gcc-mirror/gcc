// REQUIRED_ARGS: -preview=dip1000
// https://issues.dlang.org/show_bug.cgi?id=21880
extern(C++):
void spawnProcess(scope const(char*)*, File = File()) @safe
{
}

void pipeProcess(scope const(char*)* args) @safe
{
    pipeProcessImpl!spawnProcess(args);
}

void pipeProcessImpl(alias spawnFunc, Cmd)(Cmd command) @trusted
{
    spawnFunc(command);
}

struct File
{
    ~this() @safe
    {
    }
}
