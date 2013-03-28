// { dg-do assemble  }
// g++ 1.37.1 bug 900519_02

// The C++ Reference Manual says (in section 8.4.3) "A reference to a plain
// T can only be initialized with a plain T" however g++ allows the
// initialization of plain references with qualified objects in many cases.

// keywords: references, initialization, type qualifiers

extern const int cint_obj = 9;
volatile int vint_obj = 9;

void take_int_ref (int& arg) { } // { dg-message "" } referenced by errors below

int& global_int_ref0 = cint_obj;		// { dg-error "" } 
int& global_int_ref1 = vint_obj;		// { dg-error "" } 

extern const int& cint_ref;
extern volatile int& vint_ref;

void test_0 ()
{
  int& local_int_ref0 = cint_obj;		// { dg-error "" } 
  int& local_int_ref1 = vint_obj;		// { dg-error "" } 

  take_int_ref (cint_obj);			// { dg-error "" } caught
  take_int_ref (vint_obj);			// { dg-error "" } 

  take_int_ref (cint_ref);			// { dg-error "" } 
  take_int_ref (vint_ref);			// { dg-error "" } 
}

int main () { return 0; }
