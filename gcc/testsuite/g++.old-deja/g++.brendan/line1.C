// Build don't link: 
// GROUPS passed error-messages
typedef struct s S;// ERROR -  previous.*
struct S { int member:1; };  // the lineno for this should be 2, not 0// ERROR -  conflicting types.*
