// 10599 ICE(interpret.c) 

struct Bug {
    int val = 3.45;
}
int bug10599()
{
    Bug p = Bug();
    return 1;
}

static assert(bug10599());