/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-Walways-true" } */
/* Warning when addr convert to bool always gives known result.
   Ada/Pascal programmers sometimes write 0-param functions without
   (), and might as well warn on variables, too.  */

int func (void);
extern int var;
int weak_func (void) __attribute__ ((weak));
extern int weak_var __attribute__ ((weak));

int
test_func_cmp (void)
{
  if (func)      /* { dg-warning "the address of 'func'" } */
    return 1;
  if (!func)     /* { dg-warning "the address of 'func'" } */
    return 1;
  if (&var)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (!&var)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (weak_func)
    return 1;
  if (!weak_func)
    return 1;
  if (&weak_var)
    return 1;
  if (!&weak_var)
    return 1;
  return 0;
}

/* Test equality with 0 on the right hand side.  */
int
test_func_cmp_rhs_zero (void)
{
  if (func == 0)     /* { dg-warning "the address of 'func'" } */
    return 1;
  if (func != 0)     /* { dg-warning "the address of 'func'" } */
    return 1;
  if (&var == 0)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (&var != 0)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (weak_func == 0)
    return 1;
  if (weak_func != 0)
    return 1;
  if (&weak_var == 0)
    return 1;
  if (&weak_var != 0)
    return 1;
  return 0;
}

/* Test equality with 0 on the left hand side.  */
int
test_func_cmp_lhs_zero (void)
{
  if (0 == func)     /* { dg-warning "the address of 'func'" } */
    return 1;
  if (0 != func)     /* { dg-warning "the address of 'func'" } */
    return 1;
  if (0 == &var)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (0 != &var)     /* { dg-warning "the address of 'var'" } */
    return 1;
  if (0 == weak_func)
    return 1;
  if (0 != weak_func)
    return 1;
  if (0 == &weak_var)
    return 1;
  if (0 != &weak_var)
    return 1;
  return 0;
}
