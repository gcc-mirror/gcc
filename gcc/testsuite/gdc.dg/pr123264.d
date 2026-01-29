// { dg-do compile }
// { dg-options "-fdump-tree-original" }
struct S123264
{
    void infinite(const S123264) { }
    auto fn = &infinite;
}
// { dg-final { scan-tree-dump "const struct S123264" "original" } }
