// { dg-do assemble  }
// g++ 1.36.1 bug 900127_01

// g++ often fails to detect (and issue errors for) ambiguous overload
// situations.  In such cases, one of the possibilities is chosen
// (apparently arbitrarily). Errors should be issued instead.

// Cfront 2.0 passes this test.

// keywords: function overloading, ambiguity

void foo (int);
int foo (void);

typedef int (*f_ptr_t1) (void);
typedef void (*f_ptr_t2) (int);

void bar (f_ptr_t1);		// { dg-error "" } 
void bar (f_ptr_t2);		// { dg-error "" } 

void function ()
{
  bar (foo);			// { dg-error "" } ambiguous
}

int main () { return 0; }
