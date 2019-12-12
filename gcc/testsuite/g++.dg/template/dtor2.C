struct Foo
{
    template <int i>
    ~Foo() {} // { dg-error "5:destructor .Foo::~Foo\\\(\\\)." }
};

int main()
{
   Foo f;
}
