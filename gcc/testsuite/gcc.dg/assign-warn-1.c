/* Test diagnostics for bad implicit type conversions.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

#define TESTARG(ID, TL, TR) void ID##F(TL); void ID##F2(TR x) { ID##F(x); } extern int dummy
#define TESTARP(ID, TL, TR) struct { void (*x)(TL); } ID##Fp; void ID##F2(TR x) { ID##Fp.x(x); } extern int dummy
#define TESTASS(ID, TL, TR) void ID##F(TR x) { TL y; y = x; } extern int dummy
#define TESTINI(ID, TL, TR) void ID##F(TR x) { TL y = x; } extern int dummy
#define TESTRET(ID, TL, TR) TR ID##V; TL ID##F(void) { return ID##V; } extern int dummy

typedef void (*fp)(void);
typedef void (*nrfp)(void) __attribute__((noreturn));

TESTARG(fqa, nrfp, fp); /* { dg-warning "warning: passing argument 1 of 'fqaF' makes qualified function pointer from unqualified" } */
TESTARP(fqb, nrfp, fp); /* { dg-warning "warning: passing argument 1 of 'fqbFp.x' makes qualified function pointer from unqualified" } */
TESTASS(fqc, nrfp, fp); /* { dg-warning "warning: assignment makes qualified function pointer from unqualified" } */
TESTINI(fqd, nrfp, fp); /* { dg-warning "warning: initialization makes qualified function pointer from unqualified" } */
TESTRET(fqe, nrfp, fp); /* { dg-warning "warning: return makes qualified function pointer from unqualified" } */

TESTARG(ofqa, fp, nrfp);
TESTARP(ofqb, fp, nrfp);
TESTASS(ofqc, fp, nrfp);
TESTINI(ofqd, fp, nrfp);
TESTRET(ofqe, fp, nrfp);

TESTARG(qa, char *, const char *); /* { dg-warning "warning: passing argument 1 of 'qaF' discards qualifiers from pointer target type" } */
TESTARP(qb, char *, const char *); /* { dg-warning "warning: passing argument 1 of 'qbFp.x' discards qualifiers from pointer target type" } */
TESTASS(qc, char *, const char *); /* { dg-warning "warning: assignment discards qualifiers from pointer target type" } */
TESTINI(qd, char *, const char *); /* { dg-warning "warning: initialization discards qualifiers from pointer target type" } */
TESTRET(qe, char *, const char *); /* { dg-warning "warning: return discards qualifiers from pointer target type" } */

TESTARG(oqa, const char *, char *);
TESTARP(oqb, const char *, char *);
TESTASS(oqc, const char *, char *);
TESTINI(oqd, const char *, char *);
TESTRET(oqe, const char *, char *);

TESTARG(fda, fp, void *); /* { dg-warning "warning: ISO C forbids passing argument 1 of 'fdaF' between function pointer and 'void \\*'" } */
TESTARP(fdb, fp, void *); /* { dg-warning "warning: ISO C forbids passing argument 1 of 'fdbFp.x' between function pointer and 'void \\*'" } */
TESTASS(fdc, fp, void *); /* { dg-warning "warning: ISO C forbids assignment between function pointer and 'void \\*'" } */
TESTINI(fdd, fp, void *); /* { dg-warning "warning: ISO C forbids initialization between function pointer and 'void \\*'" } */
TESTRET(fde, fp, void *); /* { dg-warning "warning: ISO C forbids return between function pointer and 'void \\*'" } */

TESTARG(dfa, void *, fp); /* { dg-warning "warning: ISO C forbids passing argument 1 of 'dfaF' between function pointer and 'void \\*'" } */
TESTARP(dfb, void *, fp); /* { dg-warning "warning: ISO C forbids passing argument 1 of 'dfbFp.x' between function pointer and 'void \\*'" } */
TESTASS(dfc, void *, fp); /* { dg-warning "warning: ISO C forbids assignment between function pointer and 'void \\*'" } */
TESTINI(dfd, void *, fp); /* { dg-warning "warning: ISO C forbids initialization between function pointer and 'void \\*'" } */
TESTRET(dfe, void *, fp); /* { dg-warning "warning: ISO C forbids return between function pointer and 'void \\*'" } */

TESTARG(sua, int *, unsigned int *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'suaF' differ in signedness" } */
TESTARP(sub, int *, unsigned int *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'subFp.x' differ in signedness" } */
TESTASS(suc, int *, unsigned int *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(sud, int *, unsigned int *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(sue, int *, unsigned int *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(usa, unsigned int *, int *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'usaF' differ in signedness" } */
TESTARP(usb, unsigned int *, int *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'usbFp.x' differ in signedness" } */
TESTASS(usc, unsigned int *, int *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(usd, unsigned int *, int *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(use, unsigned int *, int *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(cua, char *, unsigned char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'cuaF' differ in signedness" } */
TESTARP(cub, char *, unsigned char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'cubFp.x' differ in signedness" } */
TESTASS(cuc, char *, unsigned char *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(cud, char *, unsigned char *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(cue, char *, unsigned char *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(uca, unsigned char *, char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'ucaF' differ in signedness" } */
TESTARP(ucb, unsigned char *, char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'ucbFp.x' differ in signedness" } */
TESTASS(ucc, unsigned char *, char *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(ucd, unsigned char *, char *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(uce, unsigned char *, char *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(csa, char *, signed char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'csaF' differ in signedness" } */
TESTARP(csb, char *, signed char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'csbFp.x' differ in signedness" } */
TESTASS(csc, char *, signed char *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(csd, char *, signed char *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(cse, char *, signed char *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(sca, signed char *, char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'scaF' differ in signedness" } */
TESTARP(scb, signed char *, char *); /* { dg-warning "warning: pointer targets in passing argument 1 of 'scbFp.x' differ in signedness" } */
TESTASS(scc, signed char *, char *); /* { dg-warning "warning: pointer targets in assignment differ in signedness" } */
TESTINI(scd, signed char *, char *); /* { dg-warning "warning: pointer targets in initialization differ in signedness" } */
TESTRET(sce, signed char *, char *); /* { dg-warning "warning: pointer targets in return differ in signedness" } */

TESTARG(cia, char *, int *); /* { dg-warning "warning: passing argument 1 of 'ciaF' from incompatible pointer type" } */
TESTARP(cib, char *, int *); /* { dg-warning "warning: passing argument 1 of 'cibFp.x' from incompatible pointer type" } */
TESTASS(cic, char *, int *); /* { dg-warning "warning: assignment from incompatible pointer type" } */
TESTINI(cid, char *, int *); /* { dg-warning "warning: initialization from incompatible pointer type" } */
TESTRET(cie, char *, int *); /* { dg-warning "warning: return from incompatible pointer type" } */

TESTARG(ica, int *, char *); /* { dg-warning "warning: passing argument 1 of 'icaF' from incompatible pointer type" } */
TESTARP(icb, int *, char *); /* { dg-warning "warning: passing argument 1 of 'icbFp.x' from incompatible pointer type" } */
TESTASS(icc, int *, char *); /* { dg-warning "warning: assignment from incompatible pointer type" } */
TESTINI(icd, int *, char *); /* { dg-warning "warning: initialization from incompatible pointer type" } */
TESTRET(ice, int *, char *); /* { dg-warning "warning: return from incompatible pointer type" } */

TESTARG(ciia, char *, int); /* { dg-warning "warning: passing argument 1 of 'ciiaF' makes pointer from integer without a cast" } */
TESTARP(ciib, char *, int); /* { dg-warning "warning: passing argument 1 of 'ciibFp.x' makes pointer from integer without a cast" } */
TESTASS(ciic, char *, int); /* { dg-warning "warning: assignment makes pointer from integer without a cast" } */
TESTINI(ciid, char *, int); /* { dg-warning "warning: initialization makes pointer from integer without a cast" } */
TESTRET(ciie, char *, int); /* { dg-warning "warning: return makes pointer from integer without a cast" } */

TESTARG(iica, int, char *); /* { dg-warning "warning: passing argument 1 of 'iicaF' makes integer from pointer without a cast" } */
TESTARP(iicb, int, char *); /* { dg-warning "warning: passing argument 1 of 'iicbFp.x' makes integer from pointer without a cast" } */
TESTASS(iicc, int, char *); /* { dg-warning "warning: assignment makes integer from pointer without a cast" } */
TESTINI(iicd, int, char *); /* { dg-warning "warning: initialization makes integer from pointer without a cast" } */
TESTRET(iice, int, char *); /* { dg-warning "warning: return makes integer from pointer without a cast" } */

struct s { int a; };

TESTARG(stria, struct s, int); /* { dg-error "error: incompatible type for argument 1 of 'striaF'" } */
TESTARP(strib, struct s, int); /* { dg-error "error: incompatible type for argument 1 of 'stribFp.x'" } */
TESTASS(stric, struct s, int); /* { dg-error "error: incompatible types in assignment" } */
TESTINI(strid, struct s, int); /* { dg-error "error: invalid initializer" } */
TESTRET(strie, struct s, int); /* { dg-error "error: incompatible types in return" } */

TESTARG(istra, int, struct s); /* { dg-error "error: incompatible type for argument 1 of 'istraF'" } */
TESTARP(istrb, int, struct s); /* { dg-error "error: incompatible type for argument 1 of 'istrbFp.x'" } */
TESTASS(istrc, int, struct s); /* { dg-error "error: incompatible types in assignment" } */
TESTINI(istrd, int, struct s); /* { dg-error "error: incompatible types in initialization" } */
TESTRET(istre, int, struct s); /* { dg-error "error: incompatible types in return" } */
