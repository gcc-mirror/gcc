// Build don't link: 
// GROUPS passed miscellaneous
// used to say invalid lvalue in `&\'
class foo {
        int     a;
    public:
        foo(int a);
};

foo::foo(int a)
{
    foo::a=a;
}

int main()
{
foo     obj(4);
}
