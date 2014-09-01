struct looksets
    {
        int lset[10];
    };

struct looksets lkst[];

flset( p )
struct looksets *p;
{
    p-- > lkst;
}
