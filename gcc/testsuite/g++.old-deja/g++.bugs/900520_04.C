// { dg-do run  }
// g++ 1.37.1 bug 900520_04

// g++ does not yet support the initialization of scalar type objects
// (including built-in arithmetic types, enum types, and pointer types)
// via constructor initialization syntax except within a call to operator
// new.

// keywords: unimplemented, syntax, initialization, scalar types

enum e_type { e_value };

typedef char *charp;

charp cp;

int global_i (1);				// { dg-bogus "" } 
double global_d (9.9);				// { dg-bogus "" } 
charp global_cp0 (cp);				// { dg-bogus "" } 
charp global_cp1 (0);				// { dg-bogus "" } 
enum e_type global_e (e_value);			// { dg-bogus "" } 

void func0 ()
{
  int local_i (1);				// { dg-bogus "" } 
  double local_d (9.9);				// { dg-bogus "" } 
  charp local_cp0 (cp);				// { dg-bogus "" } 
  charp local_cp1 (0);				// { dg-bogus "" } 
  enum e_type local_e (e_value);		// { dg-bogus "" } 
}

void func1 ()
{
  int* ip = new int (1);			// { dg-bogus "" } 
  double* dp = new double (9.9);		// { dg-bogus "" } 
  charp* cpp0 = new charp (cp);			// { dg-bogus "" } 
  charp* cpp1 = new charp (0);			// { dg-bogus "" } 
  enum e_type* ep = new e_type (e_value);	// { dg-bogus "" } 
}

int main () { return 0; }
