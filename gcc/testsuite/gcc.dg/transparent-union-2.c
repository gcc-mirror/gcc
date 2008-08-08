/* PR c/20043 */
/* { dg-compile } */
/* { dg-options "-std=gnu99" } */

typedef union { int *i; long *l; } U
  __attribute__((transparent_union));

extern void f0 (U);		/* { dg-message "note: previous declaration" } */
extern void f0 (void *);	/* { dg-error "conflicting types" } */

extern void f1 (U);		/* { dg-message "note: previous declaration" } */
extern void f1 (unsigned long);	/* { dg-error "conflicting types" } */

extern void f2 (void *);	/* { dg-message "note: previous declaration" } */
extern void f2 (U);		/* { dg-error "conflicting types" } */

extern void f3 (unsigned long);	/* { dg-message "note: previous declaration" } */
extern void f3 (U);		/* { dg-error "conflicting types" } */
