// There were two related problems here, depending on the vintage.  At
// one time:
//
//    typedef struct A { ... } A __attribute__ ((aligned (16)));
//
// would cause original_types to go into an infinite loop.  At other
// times, the attributes applied to an explicit typedef would be lost
// (check_b3 would have a negative size).

// First check that the declaration is accepted and has an effect.
typedef struct A { int i; } A __attribute__ ((aligned (16)));
int check_A[__alignof__ (A) >= 16 ? 1 : -1];

// Check that the alignment is only applied to the typedef.
struct B { int i; };
struct B b1;
typedef struct B B __attribute__((aligned (16)));
struct B b2;
B b3;
int check_b1[__alignof__ (b1) == __alignof__ (b2) ? 1 : -1];
int check_b3[__alignof__ (b3) >= 16 ? 1 : -1];
