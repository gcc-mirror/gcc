/* { dg-do compile } */
/* { dg-options "-Os" } */

int a, b;
unsigned char e;
void fn1 ()
{
    unsigned char *c=0;
    for (;; a++)
    {
        unsigned char d = *(c + b);
        for (; &e<&d; c++)
            goto Found_Top;
    }
Found_Top:
    if (0)
        goto Empty_Bitmap;
    for (;; a++)
    {
        unsigned char *e = c + b;
        for (; c < e; c++)
            goto Found_Bottom;
        c -= b;
    }
Found_Bottom:
Empty_Bitmap:
    ;
}
