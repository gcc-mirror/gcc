// { dg-do assemble  }
// prms-id: 8460

class A {
public:
  A();
  A(int) { }
  A(const A&) { }
private:
};

int main()
{
  A a;
  
  a.A(1);	// { dg-error "" } cannot find name this way
}
