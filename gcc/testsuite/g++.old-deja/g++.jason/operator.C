// General test for operator overloading permissiveness.
// Build don't link:
// Special g++ Options:

typedef __SIZE_TYPE__ size_t;

struct A {
  int operator?:(int a, int b);	   // WARNING - 
  static int operator()(int a);	   // ERROR - must be nonstatic
  static int operator+(A,A);	   // ERROR - must be nonstatic
  int operator+(int a, int b = 1); // ERROR - two errors on this line
  int operator++(char);		   // ERROR - must take 'int'
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

int operator-(int a, int b);	// ERROR - no class argument

void * operator new (A a);	// ERROR - invalid first argument
void operator delete (A a);	// ERROR - ditto

char * operator char * (int);	// ERROR - return value, nonmember
