/* { dg-do compile } */
/* { dg-options "-mabi=64 -msingle-float" } */

extern void bar (float x, short y);

void foo (int argc)
{
    short c = argc * 2;
    float a = (float)(short)c, b = 9.5;

    bar (b/a, c);
}
