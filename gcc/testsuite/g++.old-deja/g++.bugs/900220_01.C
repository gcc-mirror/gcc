// g++ 1.36.1 bug 900220_01

// Ref: 12.8

// Section 12.8 says:

//	"That is, X::operator=() will be generated only if no assignment
//	operation is explicitly declared and an object of class X is actually
//	assigned an object of class X (or an object of a class derived from X)
//	or if the address of X::operator= is taken.

// g++ does not allow you to take the address of an implicitly generated
// operator=

// keywords: operator=, implicit copy operator, operator&

struct struct0 {
  int data_member;
};

typedef struct0& (struct0::*member_func_t) (const struct0&);

member_func_t member_func;

void global_function_0 (member_func_t member_f)
{						// gets bogus error - ref from below
}

void global_function_1 ()
{
  member_func = &struct0::operator=;		// gets bogus error

  global_function_0 (&struct0::operator=);	// gets bogus error
}

int main () { return 0; }
