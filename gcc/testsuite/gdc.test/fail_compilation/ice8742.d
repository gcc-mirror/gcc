// PERMUTE_ARGS:
class C
{
    class D { }
}

void main ( )
{
    auto c = new C;
    auto d = c.new class C.D { };
}
