// PRMS Id: 5073
// Bug: g++ doesn't catch access violations in base initializers.
// Special g++ Options: -w

int r = 0;
class A {
  private:
    A() { r = 1; }		// ERROR - 
    ~A() {}			// ERROR - 
};
    
class B : public A {
  public:
    B(): A() {}			// ERROR - 
    B(const B&) {}		// ERROR - 
    ~B() { }			// ERROR - private dtor
};

main()
{
  B b;
  return r;
}
