/* { dg-do compile } */

_Float16 x;

_Float16 foo1 (_Float16 a, _Float16 b)
{
    return a + b;
}

_Float16 foo2 (_Float16 a, _Float16 b)
{
    return a * b;
}

int foo3 (_Float16 a, _Float16 b)
{
    return a > b;
}
