// { dg-do assemble  }
// GROUPS passed error-messages
typedef struct s S;// { dg-error "" }  previous.*
struct S { int member:1; };  // the lineno for this should be 2, not 0// { dg-error "" }  conflicting types.*
