/* { dg-do compile } */
/* { dg-options "-march=68000 -malign-int" } */

int a;

void f(void)
{
    a /= 3;
}
