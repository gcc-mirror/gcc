/* Test completion of incomplete types.
   There used to be a bug where some types from incomplete
   list were accidentally lost.  */
/* { dg-do compile } */
/* { dg-options "" } */

typedef struct a A[1];
typedef struct b B[1];
typedef struct c C[1];
typedef struct d D[1];
typedef struct a E;
typedef struct b F;
typedef struct c G;
typedef struct d H;
struct a { int a; };
struct c { int c; };
struct d { int d; };
struct b { int b; };
int sa = sizeof (A);
int sb = sizeof (B);
int sc = sizeof (C);
int sd = sizeof (D);
int se = sizeof (E);
int sf = sizeof (F);
int sg = sizeof (G);
int sh = sizeof (H);
