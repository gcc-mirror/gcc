void f(int* p) @nogc
{
    new(*p) int;
}
