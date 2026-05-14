/* PR tree-optimization/125290 */
/* { dg-do compile } */

int g21;
int g31, f14f16_c12;
void f14f16()
{
    short v7;
    long v9;
    long v13;
    if (g31)
    {
        g21 = 0;
        switch (v7)
        {
            case 4:
            case 66: break;
            default: __builtin_unreachable();
        }
    }
    else
    {
    lbl_bf2:
        v9 = g21;
    }
    v13 = v9;
    switch (v13)
    {
        case 55:
        case 2: goto lbl_sw12;
        default:
            if (f14f16_c12)
                goto lbl_bf2;
            else
                return;
    }
lbl_sw12:
    __builtin_unreachable();
}

