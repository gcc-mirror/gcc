/* Test C23 Checked Integer Arithmetic macros in <stdckdint.h>.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

#include <stdckdint.h>

int
main ()
{
  char a;
  bool b;
  enum E { E1, E2 } c = E1;
  int d;
  int *e;
  float f;
  double g;
  long double h;
  const int v = 42;
  volatile const short w = 5;
  ckd_add (&a, 1, 1);
  ckd_sub (&a, 1, 1);
  ckd_mul (&a, 1, 1);
  ckd_add (&b, 1, 1);		/* { dg-error "has pointer to boolean type" } */
  ckd_sub (&b, 1, 1);		/* { dg-error "has pointer to boolean type" } */
  ckd_mul (&b, 1, 1);		/* { dg-error "has pointer to boolean type" } */
  ckd_add (&c, 1, 1);		/* { dg-error "has pointer to enumerated type" } */
  ckd_sub (&c, 1, 1);		/* { dg-error "has pointer to enumerated type" } */
  ckd_mul (&c, 1, 1);		/* { dg-error "has pointer to enumerated type" } */
  ckd_add (&d, (char) 1, 1);
  ckd_sub (&d, (char) 1, 1);
  ckd_mul (&d, (char) 1, 1);
  ckd_add (&d, false, 1);
  ckd_sub (&d, false, 1);
  ckd_mul (&d, false, 1);
  ckd_add (&d, true, 1);
  ckd_sub (&d, true, 1);
  ckd_mul (&d, true, 1);
  ckd_add (&d, c, 1);
  ckd_sub (&d, c, 1);
  ckd_mul (&d, c, 1);
  ckd_add (&d, 1, (char) 1);
  ckd_sub (&d, 1, (char) 1);
  ckd_mul (&d, 1, (char) 1);
  ckd_add (&d, 1, false);
  ckd_sub (&d, 1, false);
  ckd_mul (&d, 1, false);
  ckd_add (&d, 1, true);
  ckd_sub (&d, 1, true);
  ckd_mul (&d, 1, true);
  ckd_add (&d, 1, c);
  ckd_sub (&d, 1, c);
  ckd_mul (&d, 1, c);
  ckd_add (&e, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_sub (&e, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_mul (&e, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_add (&f, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_sub (&f, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_mul (&f, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_add (&g, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_sub (&g, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_mul (&g, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_add (&h, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_sub (&h, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_mul (&h, 1, 1);		/* { dg-error "does not have pointer to integral type" } */
  ckd_add (&d, 1.0f, 1);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1.0f, 1);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1.0f, 1);	/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1.0, 1);		/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1.0, 1);		/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1.0, 1);		/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1.0L, 1);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1.0L, 1);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1.0L, 1);	/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1, 1.0f);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1, 1.0f);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1, 1.0f);	/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1, 1.0);		/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1, 1.0);		/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1, 1.0);		/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1, 1.0L);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1, 1.0L);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1, 1.0L);	/* { dg-error "does not have integral type" } */
  ckd_add (&d, (int *) 0, 1);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, (int *) 0, 1);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, (int *) 0, 1);	/* { dg-error "does not have integral type" } */
  ckd_add (&d, 1, (int *) 0);	/* { dg-error "does not have integral type" } */
  ckd_sub (&d, 1, (int *) 0);	/* { dg-error "does not have integral type" } */
  ckd_mul (&d, 1, (int *) 0);	/* { dg-error "does not have integral type" } */
  ckd_add (&v, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
  ckd_sub (&v, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
  ckd_mul (&v, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
  ckd_add (&w, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
  ckd_sub (&w, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
  ckd_mul (&w, 1, 1);		/* { dg-error "has pointer to 'const' type" } */
}
