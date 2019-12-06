// { dg-do assemble  }
// g++ 1.36.1 bug 900215_02

// g++ allows global objects (which happen to be pointers to members of some
// class X)  to be dereferenced without prefix object specifications within
// member functions of class X.

// In effect, g++ treats any dereference of a pointer-to-member which appears
// within the context of a member function (and which is not preceeded by
// either ->* or .*) as if it had been implicitly prefixed with this->*.

// The 2.0 Reference Manual only provides that such implicit prefixing
// takes place for *members* of the containing class, and *not* for
// global objects that happen to have certain types (i.e. pointer-to-member
// of the containing class).

// Also, cfront 2.0 provides implicit this-> prefixes *only* for *members*
// of the containing class.

// Cfront 2.0 passes this test.

// keywords: member pointers, this, dereference, members

struct struct0 {
  int data_member;
  void function_member ();
};

int struct0::*dmp;
int (struct0::*fmp) ();
int i;

struct struct1 {
  int data_member;

  void function_member ();
};

void struct0::function_member ()
{
  i = (this->*fmp) ();		// perfectly legal - for both cfront and g++
  i = this->*dmp;		// perfectly legal - for both cfront and g++

  i = (*fmp) ();		// { dg-error "8:invalid use of unary '\\\*' on pointer to member" } 
  i = *dmp;			// { dg-error "7:invalid use of unary '\\\*' on pointer to member" } 
}

int main () { return 0; }
