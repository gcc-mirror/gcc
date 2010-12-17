/* { dg-do compile } */
/* { dg-options "-fplan9-extensions" } */

/* When using Plan 9 extensions, a typedef can conflict with an
   anonymous field.  */

typedef struct { int a; } s1;
struct s2 { s1; int s1; };		/* { dg-error "duplicate" } */
int f(struct s2 *p) { return p->s1; }	/* { dg-error "incompatible" } */
