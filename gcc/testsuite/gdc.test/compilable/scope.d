/*
 currently fails with extra safety checks
 PERMUTE_FIXME_ARGS: -dip1000
*/

struct Cache
{
    ubyte[1] v;

    ubyte[] set(ubyte[1] v)
    {
        return this.v[] = v[];
    }
}
