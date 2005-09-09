/* { dg-do compile } */

template <int>
void f()
{
    int *t, i;
    t[i ? 0 : i];
}
