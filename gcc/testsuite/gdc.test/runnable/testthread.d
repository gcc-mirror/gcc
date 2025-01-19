// PERMUTE_ARGS:

import core.stdc.stdio;
import core.thread;

int tlsx;

class Foo
{
    int x;

    void bar()
    {
        printf("bar()\n");
        assert(tlsx == 0);
        tlsx = 5;
        Thread t = Thread.getThis();

        x = 3;
        printf("-bar()\n");
    }
}


int main()
{
    Foo f = new Foo();

    Thread t = new Thread(&f.bar);
    t.start();
    printf("t.start() done\n");
    // There is no analog for these in druntime.
    //t.pause();
    //t.resume();
    Thread.yield();
    t.join();
    printf("Done waiting\n");
    assert(f.x == 3);

    int i;
    Thread[5] tx;
    for (i = 0; i < tx.length; i++)
        tx[i] = new Thread(&f.bar);
    for (i = 0; i < tx.length; i++)
        tx[i].start();
    for (i = 0; i < tx.length; i++)
        tx[i].join();
    Thread.sleep(dur!"msecs"(1));

    printf("**Success**\n");
    return 0;
}
