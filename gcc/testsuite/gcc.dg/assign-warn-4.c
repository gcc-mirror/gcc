/* Test diagnostics for bad implicit type conversions.  Error variant.  */
/* { dg-do compile } */
/* { dg-options "-ftrack-macro-expansion=0" } */

#define TESTARG(ID, TL, TR) void ID##F(TL); void ID##F2(TR x) { ID##F(x); } extern int dummy
#define TESTARP(ID, TL, TR) struct { void (*x)(TL); } ID##Fp; void ID##F2(TR x) { ID##Fp.x(x); } extern int dummy
#define TESTASS(ID, TL, TR) void ID##F(TR x) { TL y; y = x; } extern int dummy
#define TESTINI(ID, TL, TR) void ID##F(TR x) { TL y = x; } extern int dummy
#define TESTRET(ID, TL, TR) TR ID##V; TL ID##F(void) { return ID##V; } extern int dummy

TESTARG(ciia, char *, int); /* { dg-error "passing argument 1 of 'ciiaF' makes pointer from integer without a cast" } */
TESTARP(ciib, char *, int); /* { dg-error "passing argument 1 of 'ciibFp.x' makes pointer from integer without a cast" } */
TESTASS(ciic, char *, int); /* { dg-error "assignment to 'char \\*' from 'int' makes pointer from integer without a cast" } */
TESTINI(ciid, char *, int); /* { dg-error "initialization of 'char \\*' from 'int' makes pointer from integer without a cast" } */
TESTRET(ciie, char *, int); /* { dg-error "returning 'int' from a function with return type 'char \\*' makes pointer from integer without a cast" } */

TESTARG(iica, int, char *); /* { dg-error "passing argument 1 of 'iicaF' makes integer from pointer without a cast" } */
TESTARP(iicb, int, char *); /* { dg-error "passing argument 1 of 'iicbFp.x' makes integer from pointer without a cast" } */
TESTASS(iicc, int, char *); /* { dg-error "assignment to 'int' from 'char \\*' makes integer from pointer without a cast" } */
TESTINI(iicd, int, char *); /* { dg-error "initialization of 'int' from 'char \\*' makes integer from pointer without a cast" } */
TESTRET(iice, int, char *); /* { dg-error "returning 'char \\*' from a function with return type 'int' makes integer from pointer without a cast" } */
