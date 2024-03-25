/* { dg-options " -fno-tree-ccp -fno-tree-dominator-opts -fno-tree-vrp" } */

__attribute__((noipa))
int f(int a)
{
        a &= 2;
        return a != 1;
}
int main(void)
{
        int t = f(1);
        if (!t)
        __builtin_abort();
        __builtin_printf("%d\n",t);
        return 0;
}
