// { dg-do assemble  }

class B
{
public:
    B( int t ) {}
    void f() {}
};

int g() { return 0; } // referenced below

int main()
{
    int try1;
    B( try1 ).f();   // no syntax error
    B b( g() );      // no syntax error
    B( ::g() ).f();  // no syntax error
    B( g() ).f();    // no syntax error
}
