// { dg-do assemble  }
// g++ 1.36.1 bug 900121_05

// g++ allows unions to have base types (i.e. to be "derived") and it allows
// other types to have unions as base types.  Both cases are illegal.

// g++ curently does not detect such errors.

// Cfront 2.0 passes this test.

// keywords: unions, inheritance

struct s0 {
  int s0_member;
};

union u0 : public s0 {			/* { dg-error "" } union has base class */
  int u0_member_0;
  int u0_member_1;
};

union u1 {
  int u1_member_0;
  int u1_member_1;
};

struct s1 : public u1 {			/* { dg-error "" } base class is a union */
  int s1_member_0;
};

int main () { return 0; }
