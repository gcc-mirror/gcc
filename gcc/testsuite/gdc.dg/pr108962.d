// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=108962
// { dg-do compile }
// { dg-options "-fno-exceptions -fdump-tree-original" }
extern(C) void main()
{
    final switch (0)
    {
        case 1:
            return;
    }
}
// { dg-final { scan-tree-dump-times "_d_assert_msg" 1 "original" } }
// { dg-final { scan-tree-dump-not "_d_throw" "original" } }
