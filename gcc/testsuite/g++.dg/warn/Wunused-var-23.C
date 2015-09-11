// PR c++/64383
// { dg-options "-Wunused-variable" }

struct Y
{
    ~Y();
};

struct X
{
    static Y& get();
};

int main()
{
    Y& y = X::get();  // { dg-warning "unused" }
}
