/* { dg-do compile } */

struct A
{
    double d[2];
    double foo(int i) { return d[i]; }
};

struct B : public A {};

void bar(B& b)
{
    for (int i=0; i<2; ++i)
        b.d[i] = b.foo(i);
}


