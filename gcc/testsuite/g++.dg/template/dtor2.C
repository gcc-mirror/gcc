struct Foo
{
    template <int i>
    ~Foo() {} // { dg-error "" }
};

int main()
{
   Foo f;
}
