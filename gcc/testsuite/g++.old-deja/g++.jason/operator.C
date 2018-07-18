// { dg-do assemble  }
// { dg-options "" }
// General test for operator overloading permissiveness.

typedef __SIZE_TYPE__ size_t;

struct A {
  int operator?:(int a, int b);	   // { dg-error "expected type-specifier" } 
  static int operator()(int a);	   // { dg-error "14:.static int A::operator\\(\\)\\(int\\). must be a nonstatic member function" }
  static int operator+(A,A);	   // { dg-error "14:.static int A::operator\\+\\(A, A\\). must be either a non-static member function or a non-member function" } 
  int operator+(int a, int b = 1); // { dg-error "7:.int A::operator\\+\\(int, int\\). must have either zero or one argument" }
  int operator++(char);		   // { dg-error "7:postfix .int A::operator\\+\\+\\(char\\). must have .int. as its argument" }
  void operator delete (void *);   
  void operator delete (void *, unsigned long);	
};

struct B {
  void * operator new (size_t, void *);
  int operator++(int = 0);      // { dg-error "7:.int B::operator\\+\\+\\(int\\). cannot have default arguments" } 
  int operator+ (int);
  void operator()();
  char * operator[](int);
  B * operator->();
};

int operator-(int a, int b);	// { dg-error "5:.int operator-\\(int, int\\). must have an argument of class or enumerated type" }

void * operator new (A a);	// { dg-error ".operator new. takes type .size_t." }
void operator delete (A a);	// { dg-error ".operator delete. takes type .void\\*. as first parameter" }

char * operator char * (int);	// { dg-error "return type" "ret" }
// { dg-error "8:.operator char\\*\\*\\(int\\). must be a nonstatic member function" "mem" { target *-*-* } .-1 }
