module imports.ice15176a;

import imports.ice15176b;

struct Stack(T)
{
    T[] data;
}

void func()
{
    alias ValStack = Stack!CodepointSet;
}
