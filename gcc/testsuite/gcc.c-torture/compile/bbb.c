struct looksets
    {
        int lset[10];
    };

struct looksets lkst[];

void
flset( struct looksets *p )
{
    p-- > lkst;
}
