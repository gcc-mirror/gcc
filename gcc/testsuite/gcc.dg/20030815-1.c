/* Test completion of incomplete types.
   There used to be a bug where some types from incomplete
   list were accidentally lost.  */
/* { dg-do compile } */
/* { dg-options "" } */

typedef struct a E;
typedef struct b F;
typedef struct c G;
typedef struct d H;
struct a { int a; };
struct c { int c; };
struct d { int d; };
struct b { int b; };
int se = sizeof (E);
int sf = sizeof (F);
int sg = sizeof (G);
int sh = sizeof (H);
