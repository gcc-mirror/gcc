// PERMUTE_ARGS:
enum A : typeof(null)*
{
    a = null
}

enum B : typeof(null)**
{
    a = null
}

enum C : void*
{
    a = null
}

enum D : void**
{
    a = null
}

enum NullEn : void*
{
    z = null
}

enum E : NullEn
{
    a = null
}

void main()
{
    auto a = A.a;
    auto b = B.a;
    auto c = C.a;
    auto d = D.a;
    auto e = E.a;
}
