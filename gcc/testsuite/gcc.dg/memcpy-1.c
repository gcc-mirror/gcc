/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "nasty_local" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
struct a {int a,b,c;} a;
int test(struct a a)
{
struct a nasty_local;
__builtin_memcpy (&nasty_local,&a, sizeof(a));
return nasty_local.a;
}
