/* PR tree-optimization/57442 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
short a;
unsigned b;
long c;
int d;

void f(void)
{
    b = a ? : (a = b) - c + (d - (b + b));
}

