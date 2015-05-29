/* { dg-do compile } */
/* { dg-require-weak-override "" } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void __attribute__((weak)) func(void)
{
    /* no code */
}

int main()
{
    func();
    return 0;
}

/* { dg-final { scan-tree-dump "func \\(\\);" "optimized" } } */
