// { dg-do assemble  }
// g++ 1.36.1 bug 900208_03

// The Cfront 2.0 reference manual (5.3.3) says "This type must be an
// object type; functions cannot be allocated this way...".

// g++ fails to detect (at compile time) cases where an attempt is made to
// allocate a function using new.

// keywords: operator new, function types

typedef void (func_type) ();

void global_function_0 ()
{
  new func_type;	// { dg-error "3:new cannot be applied to a function type" } missed by both cfront 2.0 and g++ 1.36.1
}

int main () { return 0; }
