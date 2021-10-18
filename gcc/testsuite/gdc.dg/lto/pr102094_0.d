// { dg-lto-do link }
module pr102094_0;

extern(C) int printf(char* s, ...);

struct S102094
{
    int a;
}

void main()
{
    S102094 x;
    void nested()
    {
        printf(cast(char*)0, x);
    }
}
