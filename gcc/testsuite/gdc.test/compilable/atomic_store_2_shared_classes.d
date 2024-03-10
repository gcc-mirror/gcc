// REQUIRED_ARGS: -preview=nosharedaccess
import core.atomic;

class Foo
{
}

void oops()
{
    auto f0 = new shared Foo;
    auto f1 = new shared Foo;
    atomicStore(f0, f1);
}
