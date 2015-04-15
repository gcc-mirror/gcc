// { dg-do assemble  }
// { dg-options "-w" }
// PRMS Id: 5073
// Bug: g++ doesn't catch access violations in base initializers.

int r = 0;
class A {
  private:
    A() { r = 1; }		// { dg-message "" } 
    ~A() {}			// { dg-message "" } 
};
    
class B : public A {
  public:
    B(): A() {}			// { dg-error "" } 
    B(const B&) {}		// { dg-error "" } 
    ~B() { }			// { dg-error "" } private dtor
};

main()
{
  B b;
  return r;
}
