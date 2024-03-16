/* PR c/111884 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* { dg-additional-options "-std=c++20" { target c++ } } */
/* { dg-additional-options "-std=c23" { target c } } */

int f(int i)
{
    int f = 1;
    return i[(unsigned char *)&f];
}

int g(int i)
{
    int f = 1;
    return i[(signed char *)&f];
}

int h(int i)
{
    int f = 1;
    return i[(char *)&f];
}
