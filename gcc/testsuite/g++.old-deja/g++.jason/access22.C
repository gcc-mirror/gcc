// PRMS Id: 8518
// Bug: Call to foo is not checked for accessibility

class A
{
  private:
    static void foo() {}	// ERROR - 
  public:
    void goo() {}
};

struct B : public A
{
    void func() { foo(); }	// ERROR - 
};

int main()
{
    B b;
    b.func();
}
