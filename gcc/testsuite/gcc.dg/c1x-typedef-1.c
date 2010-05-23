/* Test typedef redeclaration in C1X.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

/* C1X permits typedefs to be redeclared to the same type, but not to
   different-but-compatible types.  */

#include <limits.h>

typedef int TI;
typedef int TI2;
typedef TI2 TI;
typedef TI TI2;

enum e { E1 = 0, E2 = INT_MAX, E3 = -1 };
typedef enum e TE;
typedef enum e TE; /* { dg-message "previous declaration" } */
typedef int TE; /* { dg-error "with different type" } */

struct s;
typedef struct s TS;
struct s { int i; };
typedef struct s TS;

typedef int IA[];
typedef TI2 IA[]; /* { dg-message "previous declaration" } */
typedef int A2[2];
typedef TI A2[2]; /* { dg-message "previous declaration" } */
typedef IA A2; /* { dg-error "with different type" } */
typedef int A3[3];
typedef A3 IA; /* { dg-error "with different type" } */

typedef void F(int);
typedef void F(TI); /* { dg-message "previous declaration" } */
typedef void F(enum e); /* { dg-error "with different type" } */

typedef int G(void);
typedef TI G(void); /* { dg-message "previous declaration" } */
typedef enum e G(void); /* { dg-error "with different type" } */

typedef int *P;
typedef TI *P; /* { dg-message "previous declaration" } */
typedef enum e *P; /* { dg-error "with different type" } */

typedef void F2();
typedef void F2(); /* { dg-message "previous declaration" } */
typedef void F2(int); /* { dg-error "with different type" } */

void
f (void)
{
  int a = 1;
  int b = 2;
  typedef void FN(int (*p)[a]);
  typedef void FN(int (*p)[b]);
  typedef void FN(int (*p)[*]); /* { dg-message "previous declaration" } */
  typedef void FN(int (*p)[1]); /* { dg-error "with different type" } */
  typedef void FN2(int (*p)[a]);
  typedef void FN2(int (*p)[b]);
  typedef void FN2(int (*p)[*]); /* { dg-message "previous declaration" } */
  typedef void FN2(int (*p)[]); /* { dg-error "with different type" } */
  typedef int AV[a]; /* { dg-message "previous declaration" } */
  typedef int AV[b-1]; /* { dg-warning "may be a constraint violation at runtime" } */
  typedef int AAa[a];
  typedef int AAb[b-1];
  typedef AAa *VF(void); /* { dg-message "previous declaration" } */
  typedef AAb *VF(void); /* { dg-warning "may be a constraint violation at runtime" } */
}
