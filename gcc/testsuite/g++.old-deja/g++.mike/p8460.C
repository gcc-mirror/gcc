// Build don't link:
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
  
  a.A(1);	// ERROR - cannot find name this way
}
