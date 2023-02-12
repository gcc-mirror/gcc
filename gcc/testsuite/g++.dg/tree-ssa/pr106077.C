// { dg-do compile }
// { dg-options "-O2 -fno-ipa-cp -fdump-tree-optimized" }
short e,f;
static __attribute__ ((noinline))
int a(int *b)
{
        return *b;
}
static __attribute__ ((noinline))
__attribute__ ((optimize("non-call-exceptions")))
int wrap(int *b,int e, int f)
{
        e/=f;
        return a(b)+e;
}

int
t()
{
        return wrap(0,1,0);
}
// { dg-final { scan-tree-dump-not "builtin_trap" "optimized" } }
