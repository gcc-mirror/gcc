// Various struct lookup tests.  We didn't used to complain about
// the definition of C.

struct A;
typedef struct A B;		// { dg-error "previous declaration" }
struct B;			// { dg-error "conflicting declaration" }

typedef struct { int i; } C;	// { dg-error "previous declaration" }
struct C;			// { dg-error "conflicting declaration" }

struct D;
typedef struct D D;
typedef struct D { int i; } D;
typedef struct D D;
