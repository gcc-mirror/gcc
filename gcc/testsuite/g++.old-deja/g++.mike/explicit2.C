// { dg-do assemble  }

class string {
public:
  string(const char*) { } 
  explicit string(int size) { }
}; 

void foo(string) { }

string bar() {
  foo("hello");		// ok
  foo(string(2));	// ok
  foo(2);		// { dg-error "" } no implicit conversion from int to string
  string x = 2;		// { dg-error "" } no implicit conversion from int to string
  string y(2);		// ok
  foo((string)2);	// ok
  return 2;		// { dg-error "" } no implicit conversion from int to string
}

class A : string {
public:
  A() : string(2) { }	// ok
};
