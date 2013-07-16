/* { dg-options "-funroll-loops -ftracer" } */
int a, b;

int f(void)
{
    (a % b) && f();
    a = (0 || a | (a ? : 1));
}
