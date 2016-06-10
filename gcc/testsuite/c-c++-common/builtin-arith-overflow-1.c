/* Test exercising invalid calls to arithmetic overflow checking built-ins,
   including PR c/71392 - SEGV calling integer overflow built-ins with a null
   pointer, (issuing a warning for such invocations).  */
/* { dg-do compile } */
/* { dg-additional-options "-Wnonnull" }

/* Verify that calls with fewer or more than 3 arguments to the generic
   __builtin_op_overflow functions are rejected.  */

#ifndef __cplusplus
#define bool _Bool
#endif

int
generic_0 (void)
{
  int x = __builtin_add_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_add_overflow_p ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow_p ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow_p ();	/* { dg-error "too few arguments to function" } */
  return x;
}

int
generic_1 (int a)
{
  int x = __builtin_add_overflow (a);	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow (a);	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow (a);	/* { dg-error "too few arguments to function" } */

  /* Literal argument.  */
  x += __builtin_add_overflow (1);	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow (2);	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow (3);	/* { dg-error "too few arguments to function" } */
  return x;
}

int
generic_2 (int a, int b)
{
  int x = __builtin_add_overflow (a, b);/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow (a, b);	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow (a, b);	/* { dg-error "too few arguments to function" } */
  x += __builtin_add_overflow (a, 1);   /* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow (a, 2);	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow (a, 3);	/* { dg-error "too few arguments to function" } */
  x += __builtin_add_overflow (4, b);   /* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow (5, b);	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow (6, b);	/* { dg-error "too few arguments to function" } */
  return x;
}

/* Verify that calls with the correct number of arguments to the generic
   __builtin_op_overflow functions are accepted.  */

int
generic_3 (int a, int b, int c)
{
  int x = __builtin_add_overflow (a, b, &c);
  x += __builtin_sub_overflow (a, b, &c);
  x += __builtin_mul_overflow (a, b, &c);
  x += __builtin_add_overflow (a, 1, &c);
  x += __builtin_sub_overflow (a, 2, &c);
  x += __builtin_mul_overflow (a, 3, &c);
  x += __builtin_add_overflow (4, b, &c);
  x += __builtin_sub_overflow (5, b, &c);
  x += __builtin_mul_overflow (6, b, &c);
  x += __builtin_add_overflow (7, 8, &c);
  x += __builtin_sub_overflow (9, 10, &c);
  x += __builtin_mul_overflow (11, 12, &c);

  /* Verify that a null pointer to an integer is diagnosed.  */

  /* The following two are rejected due to c/71479 - error on
     __builtin_add_overflow with bool or enum pointer as last argument.

    x += __builtin_add_overflow (0, 0, (bool *)0);

    enum E { e0 };
    x += __builtin_add_overflow (0, 0, (enum E *)0);
  */

  x += __builtin_sub_overflow (0, 0, (char *)0);   /* { dg-warning "null argument" } */
  x += __builtin_add_overflow (0, 0, (short *)0);   /* { dg-warning "null argument" } */
  x += __builtin_add_overflow (a, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_sub_overflow (a, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_mul_overflow (a, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_add_overflow (a, 1, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_sub_overflow (a, 2, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_mul_overflow (a, 3, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_add_overflow (4, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_sub_overflow (5, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_mul_overflow (6, b, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_add_overflow (7, 8, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_sub_overflow (9, 10, (int *)0);   /* { dg-warning "null argument" } */
  x += __builtin_mul_overflow (11, 12, (int *)0);   /* { dg-warning "null argument" } */

  return x;
}

int
generic_4 (int a, int b, int *c, int d)
{
  int x = __builtin_add_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow (a, b, c, d, d, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_mul_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  return x;
}

/* Verify that calls with fewer or more than 3 arguments to the type
   specific forms of the __builtin_op_overflow functions are rejected.  */

int
generic_wrong_type (int a, int b)
{
  void *p = 0;
  double d = 0;
  int x = __builtin_add_overflow (a, b, p);   /* { dg-error "does not have pointer to integer type" } */
  x += __builtin_sub_overflow (a, b, &p);     /* { dg-error "does not have pointer to integer type" } */
  x += __builtin_mul_overflow (a, b, &d);     /* { dg-error "does not have pointer to integer type" } */

  /* Also verify literal arguments.  */
  x += __builtin_add_overflow (1, 1, p);   /* { dg-error "does not have pointer to integer type" } */
  x += __builtin_sub_overflow (1, 1, &p);     /* { dg-error "does not have pointer to integer type" } */
  x += __builtin_mul_overflow (1, 1, &d);     /* { dg-error "does not have pointer to integer type" } */
  return x;
}

/* Verify that calls with fewer than 2 or more than 3 arguments to
   the typed __builtin_op_overflow functions are rejected.  */
int
typed_0 (void)
{
  int x = __builtin_add_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow ();	/* { dg-error "too few arguments to function" } */
  return x;
}

int
typed_1 (int a)
{
  int x = __builtin_sadd_overflow (a);	/* { dg-error "too few arguments to function" } */
  x += __builtin_ssub_overflow (a);	/* { dg-error "too few arguments to function" } */
  x += __builtin_smul_overflow (a);	/* { dg-error "too few arguments to function" } */
  return x;
}

int
typed_2 (int a, int b)
{
  int x = __builtin_sadd_overflow (a, b);  /* { dg-error "too few arguments to function" } */
  x += __builtin_ssub_overflow (a, b);	   /* { dg-error "too few arguments to function" } */
  x += __builtin_smul_overflow (a, b);	   /* { dg-error "too few arguments to function" } */
  return x;
}

/* Exercise PR c/71392 - SEGV calling integer overflow built-ins with
   a null pointer.  Verify that calls with a null argument are diagnosed
   with -Wnonnull.  */

int
typed_3_null (int a, int b)
{
  int x = 0;

  x += __builtin_sadd_overflow (a, b, (int *)0); /* { dg-warning "null argument" } */
  x += __builtin_uadd_overflow (a, b, (unsigned *)0); /* { dg-warning "null argument" } */

  x += __builtin_saddl_overflow (a, b, (long *)0); /* { dg-warning "null argument" } */
  x += __builtin_uaddl_overflow (a, b, (unsigned long *)0); /* { dg-warning "null argument" } */

  x += __builtin_saddll_overflow (a, b, (long long *)0); /* { dg-warning "null argument" } */
  x += __builtin_uaddll_overflow (a, b, (unsigned long long *)0); /* { dg-warning "null argument" } */


  x += __builtin_ssub_overflow (a, b, (int *)0); /* { dg-warning "null argument" } */
  x += __builtin_usub_overflow (a, b, (unsigned *)0); /* { dg-warning "null argument" } */

  x += __builtin_ssubl_overflow (a, b, (long *)0); /* { dg-warning "null argument" } */
  x += __builtin_usubl_overflow (a, b, (unsigned long *)0); /* { dg-warning "null argument" } */

  x += __builtin_ssubll_overflow (a, b, (long long *)0); /* { dg-warning "null argument" } */
  x += __builtin_usubll_overflow (a, b, (unsigned long long *)0); /* { dg-warning "null argument" } */


  x += __builtin_smul_overflow (a, b, (int *)0); /* { dg-warning "null argument" } */
  x += __builtin_umul_overflow (a, b, (unsigned *)0); /* { dg-warning "null argument" } */

  x += __builtin_smull_overflow (a, b, (long *)0); /* { dg-warning "null argument" } */
  x += __builtin_umull_overflow (a, b, (unsigned long *)0); /* { dg-warning "null argument" } */

  x += __builtin_smulll_overflow (a, b, (long long *)0); /* { dg-warning "null argument" } */
  x += __builtin_umulll_overflow (a, b, (unsigned long long *)0); /* { dg-warning "null argument" } */

  return x;
}

int
typed_4 (int a, int b, int *c, int d)
{
  int x = __builtin_sadd_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_ssub_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_smul_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  return x;
}

int
f2 (int a, int b, int *c, int d)
{
  int x = __builtin_add_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow (a, b, c, d, d, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_mul_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_add_overflow_p (a, b, d, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow_p (a, b, d, d, 1, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_mul_overflow_p (a, b, d, d);	/* { dg-error "too many arguments to function" } */

  return x;
}

enum E { e0 = 0, e1 = 1 };

int
f3 (float fa, int a, _Complex long int ca, double fb, void *pb, int b, enum E eb, bool bb, int *c)
{
  int x = __builtin_add_overflow (fa, b, c);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow (ca, b, c);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow (a, fb, c);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_add_overflow (a, pb, c);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow (a, eb, c);
  x += __builtin_mul_overflow (a, bb, c);
  x += __builtin_add_overflow_p (fa, b, a);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (ca, b, eb);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow_p (a, fb, bb);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_add_overflow_p (a, pb, a);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (a, eb, eb);
  x += __builtin_mul_overflow_p (a, bb, bb);
  x += __builtin_add_overflow_p (a, b, fa);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (a, b, ca);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow_p (a, b, c);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  return x;
}

int
f4 (float *fp, double *dp, _Complex int *cp, enum E *ep, bool *bp, long long int *llp)
{
  int x = __builtin_add_overflow (1, 2, fp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_sub_overflow (1, 2, dp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_mul_overflow (1, 2, cp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_add_overflow (1, 2, ep);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_sub_overflow (1, 2, bp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_mul_overflow (1, 2, llp);
  return x;
}
