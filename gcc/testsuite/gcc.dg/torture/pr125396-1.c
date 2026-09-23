/* { dg-do compile } */
/* PR tree-optimization/125396 */


char g7;
int g13, g26;
int g() __attribute__((returns_twice));
void f8()
{
    int arr1[2];
    int c2, c4;
    char v6;
    int sj12;
    int sj14;
lbl_br1:
    if (g13) goto lbl_br3;
    if (c4)
    {
        c4 = 0;
        goto lbl_br6;
    }
    switch (v6)
    case 1:
        c4 = 1;
    sj14 = g();
    g13 = sj12;
lbl_br3:
    arr1[0] = 0;
    sj12 = g();
    c2 = g7;
    g26 = 5;
    if (c2) __builtin_unreachable();
lbl_br6:
    sj12 = arr1[0];
    arr1[0] = 9;
    if (g26) f8();
    goto lbl_br1;
}
