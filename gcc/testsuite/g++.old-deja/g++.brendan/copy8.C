// GROUPS passed copy-ctors
/*
This report is for GCC 2.3.3 running on a Sun/4.  The bug is that when
a class instance is passed-by-value, GCC does not correctly copy the value.
At the end of this report is an example program that demonstrates the bug.  
It should print:

	construct A('x')
	copy A('x')
	destruct A('x')
	destruct A('x')

and in fact does for IBM's xlC C++.  However, for GCC 2.3.3, it fails
to print the second line ["copy A('x')"], which indicates that it failed
to call the copy-constructor for class A when it should have.  Below is a 
typescript that lists the program, shows how I compiled it, and shows the 
incorrect output.
*/

extern "C" void printf (char *, ...);
extern "C" void exit (int);

int count = 0;

void
die (int x)
{
  if (x != ++count)
    {
      printf ("FAIL\n");
      exit (1);
    }
}

class A { // Class with explicit & instrumented copy-constructor and destructor.
public:
    const char * id;
    A( const char * id1 ) : id(id1) { die (1); }

    // Copy constructor
    A( const A& a ) : id(a.id) { die (2); }

    // Destructor
    ~A() { count++; if (count != 3 && count != 4) die (-1); }
};

class X { // Class without explicit copy-constructor
private:
    A a;
public:
    X( const char * id ) : a(id) {}
};

void Func( X x ) {      // Function with call-by-value argument
}

int
main() {
    X x("x");           // Construct instance of x.

    // The next line should call the copy-constructor for X since x is
    // being passed by value.  For GCC 2.3.3 on a Sun/4, it does not.
    Func(x);

    printf ("PASS\n");
    return 0;
}
