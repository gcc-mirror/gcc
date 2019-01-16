// 10273 - ICE in CTFE

struct Bug10273 {
    int val = 3.45;
}
int bug10273()
{
    Bug10273 p;
    return 1;
}

static assert(bug10273());
