import core.memory;

void main()
{
    {
        int[][] a = new int[][](2, 2);
        assert(!(GC.getAttr(a.ptr) & GC.BlkAttr.NO_SCAN));
        assert(GC.getAttr(a[0].ptr) & GC.BlkAttr.NO_SCAN);
    }
    {
        void*[][] a = new void*[][](2, 2);
        assert(!(GC.getAttr(a.ptr) & GC.BlkAttr.NO_SCAN));
        assert(!(GC.getAttr(a[0].ptr) & GC.BlkAttr.NO_SCAN));
    }
    {
        int[][][] a = new int[][][](2, 2);
        assert(!(GC.getAttr(a.ptr) & GC.BlkAttr.NO_SCAN));
        assert(!(GC.getAttr(a[0].ptr) & GC.BlkAttr.NO_SCAN));
        assert(a[0][0].ptr is null);
    }
}
