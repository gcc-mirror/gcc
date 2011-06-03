/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize -fdump-tree-vect-details -fno-tree-fre" } */

volatile unsigned char g_324[4] = {0, 1, 0, 1};
void foo (int);
int x, y;
void func_81(void)
{
    int l_466, l_439[7] = {0}, g_97;
lbl_473:
    if (x) {
        for (g_97 = 0; (g_97 < 4); ++g_97) {
            if (y)
              goto lbl_473;
            g_324[g_97];
            l_466 = l_439[g_97];
        }
        foo(l_466);
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
