// { dg-do assemble  }
// { dg-options "" }
// General test for operator overloading permissiveness.

typedef __SIZE_TYPE__ size_t;

struct A {
  int operator?:(int a, int b);	   // { dg-warning "" } 
  static int operator()(int a);	   // { dg-error "" } must be nonstatic
  static int operator+(A,A);	   // { dg-error "" } must be nonstatic
  int operator+(int a, int b = 1); // { dg-error "" } two errors on this line
  int operator++(char);		   // { dg-error "" } must take 'int'
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

int operator-(int a, int b);	// { dg-error "" } no class argument

void * operator new (A a);	// { dg-error "" } invalid first argument
void operator delete (A a);	// { dg-error "" } ditto

char * operator char * (int);	// { dg-error "" } return value, nonmember
