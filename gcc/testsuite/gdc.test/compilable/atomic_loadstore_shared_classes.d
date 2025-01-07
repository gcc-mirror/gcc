// REQUIRED_ARGS: -preview=nosharedaccess
import core.atomic;

class Foo
{
}

shared Foo toLoad;

void oops()
{
    auto f0 = new shared Foo;
    auto f1 = new shared Foo;
    atomicStore(f0, f1);

    // https://issues.dlang.org/show_bug.cgi?id=24846
    shared(Foo) f2 = atomicLoad(toLoad);
}
