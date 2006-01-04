/* PR c/25559 */
/* { dg-do compile } */

#define vs(n) __attribute__((vector_size (n)))
int vs (-1) a;			/* { dg-warning "attribute ignored" } */
int vs (0) b;			/* { dg-error "zero vector size" } */
int vs (1) c;			/* { dg-error "multiple of component size" } */
int vs (sizeof (int) / 2) d;	/* { dg-error "multiple of component size" } */
int vs (sizeof (int)) e;
int vs (sizeof (int) * 2) f;
