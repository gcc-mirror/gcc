// { dg-do assemble  }
// PRMS Id: 8518
// Bug: Call to foo is not checked for accessibility

class A
{
  private:
    static void foo() {}	// { dg-error "" } 
  public:
    void goo() {}
};

struct B : public A
{
    void func() { foo(); }	// { dg-error "" } 
};

int main()
{
    B b;
    b.func();
}
