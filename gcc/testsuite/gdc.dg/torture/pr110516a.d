// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110516
// { dg-do compile }
// { dg-options "-fno-moduleinfo -fdump-tree-optimized" }
void fn110516(ubyte* ptr)
{
    import core.volatile : volatileLoad;
    volatileLoad(ptr);
    volatileLoad(ptr);
    volatileLoad(ptr);
    volatileLoad(ptr);
}
// { dg-final { scan-tree-dump-times " ={v} " 4 "optimized" } }
