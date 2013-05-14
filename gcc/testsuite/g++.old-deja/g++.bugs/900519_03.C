// { dg-do assemble  }
// g++ 1.37.1 bug 900519_03

// The C++ Reference Manual says (in section 8.4.3) "A reference to a 
// volatile T can be initialized with a volatile T or a plain T but not a
// const T.  A reference to a const T can be initialized with a const T or
// a plain T or something that can be converted into a plain T, but not a
// volatile T."

// g++ fails to disgnose such errors in most cases.

// keywords: references, initialization, type qualifiers

extern const int cint_obj;
extern volatile int vint_obj;

void take_cint_ref (const int& arg) { }	// { dg-message "" } 
void take_vint_ref (volatile int& arg) { } // { dg-message "" } 

const int& global_cint_ref2 = vint_obj;		// { dg-error "" } 

volatile int& global_vint_ref1 = cint_obj;	// { dg-error "" } 

extern const int& extern_cint_ref;
extern volatile int& extern_vint_ref;

void test_0 ()
{
  const int& local_cint_ref2 = vint_obj;	// { dg-error "" } 

  volatile int& local_vint_ref1 = cint_obj;	// { dg-error "" } 
} 

void test_1 ()
{
  take_cint_ref (vint_obj);			// { dg-error "" } 

  take_vint_ref (cint_obj);			// { dg-error "" } caught
}

void test_2 ()
{
  take_cint_ref (extern_vint_ref);		// { dg-error "" } 

  take_vint_ref (extern_cint_ref);		// { dg-error "" } 
}

int main () { return 0; }
