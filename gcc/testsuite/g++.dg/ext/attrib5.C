// There were two related problems here, depending on the vintage.  At
// one time:
//
//    typedef struct A { ... } A __attribute__ ((aligned (16)));
//
// would cause original_types to go into an infinite loop.  At other
// times, the attributes applied to an explicit typedef would be lost
// (check_b2 would have a negative size).

// First check that the declaration is accepted and has an effect.
typedef struct A { int i; } A __attribute__ ((aligned (16)));
int check_A[__alignof__ (A) >= 16 ? 1 : -1];

// Check that the alignment is only applied to the typedef.
struct B { int i; };
namespace N { typedef B B; };
typedef struct B B __attribute__((aligned (16)));
N::B b1;
B b2;
int check_b1[__alignof__ (b1) == __alignof__ (int) ? 1 : -1];
int check_b2[__alignof__ (b2) >= 16 ? 1 : -1];

// The fix for this case involved a change to lookup_tag.  This 
// bit just checks against a possible regression.
namespace N { struct C; };
typedef struct N::C C;		// { dg-error "previous declaration" }
struct C;			// { dg-error "conflicting types" }
