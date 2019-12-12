struct A(T)
{
    mixin B!(T, A!(T));
}

A!(int) x;


