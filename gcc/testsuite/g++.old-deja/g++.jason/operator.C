// { dg-do assemble  }
// { dg-options "" }
// General test for operator overloading permissiveness.

typedef __SIZE_TYPE__ size_t;

struct A {
  int operator?:(int a, int b);	   // { dg-error "expected type-specifier" } 
  static int operator()(int a);	   // { dg-error "must be a nonstatic member" }
  static int operator+(A,A);	   // { dg-error "either a non-static member" } 
  int operator+(int a, int b = 1); // { dg-error "either zero or one" }
  int operator++(char);		   // { dg-error "must take 'int'" } 
  void operator delete (void *);   
  void operator delete (void *, unsigned long);	
};

struct B {
  void * operator new (size_t, void *);
  int operator++(int = 0);
  int operator+ (int);
  void operator()();
  char * operator[](int);
  B * operator->();
};

int operator-(int a, int b);	// { dg-error "argument of class or" }

void * operator new (A a);	// { dg-error "first parameter" }
void operator delete (A a);	// { dg-error "first parameter" }

char * operator char * (int);	// { dg-error "return type" "ret" }
// { dg-error "nonstatic member function" "mem" { target *-*-* } .-1 }
