/* Type definitions that are used by multiple tests.  */

#define DEFS(NAME,TYPEM)					\
typedef struct { TYPEM a; } S##NAME##1;				\
typedef struct { TYPEM a; TYPEM b; } S##NAME##2;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; } S##NAME##3;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; }		\
	       S##NAME##4;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e; }	\
	       S##NAME##5;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
  		 TYPEM f; } S##NAME##6;				\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; } S##NAME##7;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; } S##NAME##8;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; }		\
	       S##NAME##9;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j; }	\
	       S##NAME##10;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; } S##NAME##11;			\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; } S##NAME##12;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; } S##NAME##13;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; }		\
	        S##NAME##14;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; TYPEM o; }	\
	       S##NAME##15;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; TYPEM o;	\
		 TYPEM p; } S##NAME##16;
