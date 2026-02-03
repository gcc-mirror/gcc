/* { dg-do compile } */
/* { dg-options "-mabi=64 -O2 -march=octeon2" } */

#define COUNT 10

typedef unsigned short u16;
typedef unsigned int   u32;

typedef struct NeedleAddress
{
  u16   nId;
  u16   mId;
} NeedleAddress;

u32 __attribute__ ((noinline)) prepareNeedle(const u16 upper, const u16 lower)
{
    u32 needleAddress = 0;
    NeedleAddress *const addr = (NeedleAddress*)(&needleAddress);
    addr->mId = upper;
    addr->nId = lower;
    return needleAddress;
}

const u32* __attribute__ ((noinline)) findNeedle(const u32 needle, const u32* begin, const u32* end)
{
    while ( begin != end && needle != *begin )
    {
        ++begin;
    }
    return begin;
}

int main()
{
    u32 needle = prepareNeedle(0xDCBA, 0xABCD);

    u32 haystack[COUNT] = {};
    for (int i = 0; i < COUNT; i++)
        haystack[i] = needle;

    const u32* result = findNeedle(needle, haystack, haystack + COUNT);
    if (result == haystack + COUNT)
        __builtin_abort ();
    return 0;
}
