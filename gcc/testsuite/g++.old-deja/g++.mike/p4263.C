// prms-id: 4263

enum OT {A_type, B_Type};
enum AT {A, B};

/* These are not ok. */
OT t = A;	// ERROR - 
OT e2 = 1;	// ERROR - 
OT e3 = 1.1;	// ERROR - 

/* These are ok. */
int i = A;
double d = A;
OT e4 = A_type;
