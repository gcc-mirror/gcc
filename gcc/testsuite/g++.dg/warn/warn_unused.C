// { dg-do compile }
// { dg-options -Wunused }

struct __attribute__((warn_unused)) Test
{
    Test();
    ~Test();
    void use();
};

struct TestNormal
{
    TestNormal();
};

int main()
{
   Test unused;         // { dg-warning "unused variable" }
   Test used;           // { dg-bogus "unused variable" }
   TestNormal normal;   // { dg-bogus "unused variable" }
   used.use();
}
