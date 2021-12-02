module two;

import one;

struct ET(bool a)
{
    enum e = BB.MAX_NUM_FIBERS;
}

alias Event = ET!false;

struct TWOR(size_t M)
{
    Event e;

    void open()
    {
        bb.foo();
    }
}
