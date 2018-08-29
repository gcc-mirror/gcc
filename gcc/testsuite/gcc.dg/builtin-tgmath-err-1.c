/* Test __builtin_tgmath: errors that indicate a bad definition of a
   type-generic macro rather than bad arguments in a call to it.  */
/* { dg-do compile } */
/* { dg-options "" } */

void *p;
double d;
double unprototyped_d ();
long double unprototyped_ld ();
double variadic_d (double, ...);
long double variadic_ld (long double, ...);
double no_arguments_d (void);
long double no_arguments_ld (void);
double f_d (double);
long double f_ld (long double);
double many_args (double, double, double, double);
int f_i_d (double);
_Complex int f_ci_d (double);
void * f_p_d (double);
double f_d_i (int);
double f_d_ci (_Complex int);
double f_d_p (void *);
long double f_ld_d (double);
_Complex double f_cd_d (double);
double f_d_f (float);
double f_d_dd (double, double);
long double f_ld_ldld (long double, long double);
float f_f_fd (float, double);

void
test (void)
{
  /* Arguments individually invalid or no consistent number of
     arguments followed by those arguments.  */
  __builtin_tgmath (); /* { dg-error "too few arguments" } */
  __builtin_tgmath (f_d); /* { dg-error "too few arguments" } */
  __builtin_tgmath (f_d, f_ld); /* { dg-error "too few arguments" } */
  __builtin_tgmath (many_args, many_args, many_args); /* { dg-error "too few arguments" } */
  __builtin_tgmath (many_args, d, d, d, d); /* { dg-error "too few arguments" } */
  __builtin_tgmath (f_ld, many_args, d); /* { dg-error "has wrong number of arguments" } */
  __builtin_tgmath (unprototyped_d, unprototyped_ld, d); /* { dg-error "is unprototyped" } */
  __builtin_tgmath (f_d, unprototyped_ld, d); /* { dg-error "is unprototyped" } */
  __builtin_tgmath (variadic_d, variadic_ld, d); /* { dg-error "variable arguments" } */
  __builtin_tgmath (f_d, variadic_ld, d); /* { dg-error "variable arguments" } */
  __builtin_tgmath (p, p, p); /* { dg-error "is not a function pointer" } */
  __builtin_tgmath (f_d, p, p); /* { dg-error "is not a function pointer" } */
  __builtin_tgmath (no_arguments_d, no_arguments_d, no_arguments_ld); /* { dg-error "has no arguments" } */
  __builtin_tgmath (f_d, no_arguments_d, no_arguments_ld); /* { dg-error "has no arguments" } */

  /* Invalid varying types of arguments.  */
  __builtin_tgmath (f_i_d, f_ld, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_ci_d, f_ld, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_p_d, f_ld, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_ld, f_i_d, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_ld, f_ci_d, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_ld, f_p_d, 0); /* { dg-error "invalid type-generic return type" } */
  __builtin_tgmath (f_d_i, f_ld, 0); /* { dg-error "invalid type-generic type for argument" } */
  __builtin_tgmath (f_d_ci, f_ld, 0); /* { dg-error "invalid type-generic type for argument" } */
  __builtin_tgmath (f_d_p, f_ld, 0); /* { dg-error "invalid type-generic type for argument" } */
  __builtin_tgmath (f_ld, f_d_i, 0); /* { dg-error "invalid type-generic type for argument" } */
  __builtin_tgmath (f_ld, f_d_ci, 0); /* { dg-error "invalid type-generic type for argument" } */
  __builtin_tgmath (f_ld, f_d_p, 0); /* { dg-error "invalid type-generic type for argument" } */

  /* Arguments same type.  */
  __builtin_tgmath (f_d, f_d, 0); /* { dg-error "all have the same type" } */

  /* Missing or invalid type-generic parameter.  */
  __builtin_tgmath (f_d, f_ld_d, 0); /* { dg-error "lack type-generic parameter" } */
  __builtin_tgmath (f_d, f_ld, f_cd_d, 0); /* { dg-error "lack type-generic parameter" } */
  __builtin_tgmath (f_d, f_ld, f_d, 0); /* { dg-error "duplicate type-generic parameter type" } */

  /* Variation not consistent with the identified type-generic
     parameter.  */
  __builtin_tgmath (f_d, f_ld, f_d_f, 0); /* { dg-error "bad return type for function argument" } */
  __builtin_tgmath (f_d_dd, f_ld_ldld, f_f_fd, 0, 0); /* { dg-error "bad type for argument" } */
}
