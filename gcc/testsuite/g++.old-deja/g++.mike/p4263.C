// { dg-do assemble  }
// prms-id: 4263

enum OT {A_type, B_Type};
enum AT {A, B};

/* These are not ok. */
OT t = A;	// { dg-error "" } 
OT e2 = 1;	// { dg-error "" } 
OT e3 = 1.1;	// { dg-error "" } 

/* These are ok. */
int i = A;
double d = A;
OT e4 = A_type;
