// { dg-do run  }
// { dg-options "-O" }
// PRMS Id: 10776

extern "C" int printf (const char *, ...);

class Foo 
{
  public:
    Foo(int n) : n_(n) { }
    int f() { return n_; }
    
    int badTest();
    int goodTest();
    
  private:

    int n_;
};

int Foo::badTest()
{
    try {
	throw int(99);
    }

    catch (int &i) {
	n_ = 16;
    }

    return n_;
        // On the sparc, the return will use a ld [%l0],%i0 instruction.
        // However %l0 was clobbered at the end of the catch block.  It was
        // used to do an indirect call.
}


int Foo::goodTest()
{
    int	n;

    try {
	throw int(99);
    }

    catch (int &i) {
	n = 16;
    }

    return n_;
        // The return will use a ld [%l2],%i0 instruction.  Since %l2
        // contains the "this" pointer this works.
}

int main() 
{
    Foo foo(5);
    foo.goodTest();
    foo.badTest();

    // the badTest will have failed
    printf ("PASS\n");
}
