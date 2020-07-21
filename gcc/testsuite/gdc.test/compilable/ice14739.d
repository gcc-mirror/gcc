// REQUIRED_ARGS: -o-

void main(string[] args)
{
    immutable int a;
    immutable int b;
    S!a sa;
    S!b sb;
    C!a ca;
    C!b cb;
}

struct S(alias a)
{
}

class C(alias a)
{
}
