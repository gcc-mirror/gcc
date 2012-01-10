// Various struct lookup tests.  We didn't used to complain about
// the definition of C.

struct A;
typedef struct A B;		// { dg-message "previous declaration" }
struct B;			// { dg-error "using typedef-name" }

typedef struct { int i; } C;	// { dg-message "previous declaration" }
struct C;			// { dg-error "using typedef-name" }

struct D;
typedef struct D D;
typedef struct D { int i; } D;
typedef struct D D;
