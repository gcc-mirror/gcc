struct A
{
    ~A();
};
int foo(A);
void bar()
{
    A a;
    asm("" : : "r"(foo(a)) );//<-- cleanup needed here.
}

