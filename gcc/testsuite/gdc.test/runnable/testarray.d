// PERMUTE_ARGS:
// REQUIRED_ARGS:

import core.memory;

// see https://forum.dlang.org/thread/4BB6296E.6050506@digitalmars.com for more info

// failure case for bug fixed by druntime rev 282
// how it works:
// The erroneous code used the first element in the LRU cache to determine the
// length of the block, instead of looking at the current blocks blockinfo.  So
// we need a somewhat alignment of moons to get this to fail.  First, we create
// the perfect storm: 1) a long blockinfo at the beginning (size = 32), 2) the
// second blockinfo is smaller (16 bytes) and 3) a block of memory that is
// exactly 16 bytes past the element in the 2nd block.  While this seems kind
// of unlikely, in code that uses a lot of appending, it can easily happen.
//
// The bug causes the array appender to think that a pointer in the 16 bytes
// past the second blockinfo is actually in the second blockinfo.  This is
// because it uses the length from the first blockinfo for checking (which is
// 32, so since the pointer is only 16 bytes into the block, it is considered
// inside).
// On top of all this, the third block must be marked as containing pointers,
// and the second block not containing pointers.  The reason is because, it's
// impossible for the runtime to append in place, since the outside pointer
// can't possibly match the array length in the block.  But because the runtime
// copies the blockinfo's attributes when reallocating, it copies the
// attributes from the *wrong* block.  The only way to make this cause a
// problem would be to then collect memory, which should incorrectly deallocate
// data that the array containing pointers points to, and then use those
// pointers.  However, for our purposes, we know this is possible, but we can
// deterministically check the attributes of the array after appending and
// verify that they are wrong.
//
void main()
{
    // fill up the cache to make it wrap, The block info cache has 8 elements,
    // and the first element is not the first one filled, so we want to wrap to
    // that first element we want to fill in
    for(int i = 0; i < 7; i++)
    {
        auto n = new int[1];
        n ~= 1;
    }

    // this will allocate a 32-byte block, and appending will insert it into
    // the cache at the beginning.
    auto y = new int[4];
    y ~= 1;

    // now, allocate a 16 byte block with pointers, this will be our block that
    // gets corrupted.  The current GC allocates down, so we allocate this one
    // first.
    auto x = new char[][1];

    // this block contains no pointers, and appending will insert it into the
    // second element of the cache
    y = new int[1];
    y ~= 1;

    // verify the noscan flag is 0 on the pointer-containing blocks
    assert((GC.getAttr(x.ptr) & GC.BlkAttr.NO_SCAN) == 0);
    x ~= "hello".dup; // this should leave the attributes alone
    assert((GC.getAttr(x.ptr) & GC.BlkAttr.NO_SCAN) == 0); // fails on 2.042
}
