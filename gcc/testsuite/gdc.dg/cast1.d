// { dg-do compile }
float bug8060(float x)
{
    int i = *cast(int*)&x;
    ++i;
    return *cast(float*)&i;
}
