//test 2
struct A {};
void f()
{
        struct A;
        throw *(new A);
}
