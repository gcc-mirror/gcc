// Build don't link: 
// GROUPS passed visibility
extern "C" int printf( const char *, ...);

class B {
public:
        B() { };
        virtual ~B() { printf( "B::~B\n"); };
};

class D : public B {
public:
        virtual ~D() { printf( "D::~D\n"); };
  void operator = ( int i) { this->~B(); }// ERROR - D has no ~B part to it
};

int
main()
{
        D * pd = new D;
        B * pb = pd;
        delete pb;
        return 0;
};
