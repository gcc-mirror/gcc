// The hack for PR c++/44909 breaks this testcase.  We need feedback
// from the C++ committee to know how to proceed.
// { dg-options -std=c++11 }

struct A
{
  A();
  A(A&);
};

struct B;
struct BP
{
  BP(const B&);
};

struct B
{
  B();
  B(B&&);
  B(const BP&);
};

// If B(B&&) suppresses the B copy constructor, then copying the B
// subobject of C should use B(const BP&).  But we ignore that constructor
// in order to break the cycle in 44909.  Perhaps the move ctor shouldn't
// suppress the copy ctor?
// As of DR 1082, it doesn't suppress it.
struct C: A, B { };		// { dg-error "use of deleted" }

C c;
C c2(c);			// { dg-error "deleted" }
