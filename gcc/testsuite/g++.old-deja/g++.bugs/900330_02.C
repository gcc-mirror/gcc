// { dg-do assemble  }
// g++ 1.37.1 bug 900330_02

// The C++ Reference Manual says in section 13.1:

// "Two function declarations of the same name refer to the same function
// if they are in the same scope and have identical argument types.  A
// function member of a derived class is *not* in the same scope as a function
// member of the same name in a base class."

// g++ fails to correctly detect the error indicated.

// Cfront 2.0 passes this test.

// keywords: function, member, overloading, hiding

struct B {
  int f(int);
};

struct D : public B {
  int f(struct B);		// { dg-message "candidates" } referred to below
};

void h(D* pd)
{
  pd->f(1);		// { dg-error "no matching" } D::f(struct B) hides B::f(int)
}

int main () { return 0; }
