/* { dg-do preprocess } */

/* Tests for line numbering around function-like macro calls.
   Bug found by Mark Mitchell.  */

#define f(x) x
#define g f

f (3);
#error here	/* { dg-error "here" "case 0" } */

f
  (3);
#error here	/* { dg-error "here" "case 1" } */

(f
  )(3);
#error here	/* { dg-error "here" "case 2" } */

g
  (3);
#error here	/* { dg-error "here" "case 3" } */

(g
  )(3);
#error here	/* { dg-error "here" "case 4" } */

f /* some
     text */  (3);
#error here	/* { dg-error "here" "case 5" } */

(g /* some
      text */ )(3);
#error here	/* { dg-error "here" "case 6" } */
