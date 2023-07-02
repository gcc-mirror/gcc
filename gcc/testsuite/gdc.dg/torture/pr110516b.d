// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110516
// { dg-do compile }
// { dg-options "-fno-moduleinfo -fdump-tree-optimized" }
void fn110516(ubyte* ptr)
{
    import core.volatile : volatileStore;
    volatileStore(ptr, 0);
    volatileStore(ptr, 0);
    volatileStore(ptr, 0);
    volatileStore(ptr, 0);
}
// { dg-final { scan-tree-dump-times " ={v} " 4 "optimized" } }
