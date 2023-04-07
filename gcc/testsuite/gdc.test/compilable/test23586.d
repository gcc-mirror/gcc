// https://issues.dlang.org/show_bug.cgi?id=23586

int test23686a(int x)
{
    switch(x)
    {
        case 0:
            goto Bar;

        Bar:
        default:
            auto y = 6;
            return y;
    }
}

int test23686b(int x)
{
    switch(x)
    {
        case 0:
        Bar:
        case 1:
        case 2:
            auto y = 7;
            return y;

        default:
            goto Bar;
    }
}

static assert(test23686a(0) == 6);
static assert(test23686b(3) == 7);
