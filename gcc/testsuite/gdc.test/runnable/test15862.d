// https://issues.dlang.org/show_bug.cgi?id=15862

/*
PERMUTE_ARGS:
REQUIRED_ARGS: -O -release
*/


int* p() pure nothrow {return new int;}
int[] a() pure nothrow {return [0];}
Object o() pure nothrow {return new Object;}

auto pa() pure nothrow {return new int;}

void main()
{
    {
        int* p1 = p();
        int* p2 = p();

        if (p1 is p2) assert(0);

        int[] a1 = a();
        int[] a2 = a();

        if (a1 is a2) assert(0);

        Object o1 = o();
        Object o2 = o();

        if (o1 is o2) assert(0);
    }
    {
        auto p1 = pa();
        auto p2 = pa();

        if (p1 is p2) assert(0);
    }
}
