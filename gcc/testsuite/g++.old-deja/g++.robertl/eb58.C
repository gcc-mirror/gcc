// { dg-do compile  }
// { dg-options "-w -fpermissive" }
// Test for g++ array init extension 

class A {
public:
        A(int i) {}
private:
        A( const A & ) {}
};

main()
{
  A *list = new A[10](4); // { dg-error "parenthesized|could not convert" }
}
