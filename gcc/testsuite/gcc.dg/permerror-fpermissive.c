/* { dg-options "-std=gnu17 -fpermissive" } */

/* Overview test for C permerrors.  This test should be kept in sync with the
   other permerror-* tests.  If new permerrors are added, test cases should be
   added to this and the other files.  */

void
implicit_function_declaration (void)
{
  f1 (); /* { dg-warning "'f1' \\\[-Wimplicit-function-declaration\\\]" } */
}

extern implicit_int_1; /* { dg-warning "'implicit_int_1' \\\[-Wimplicit-int\\\]" } */
typedef implicit_int_2; /* { dg-warning "'implicit_int_2' \\\[-Wimplicit-int\\\]" } */
extern implicit_int_3 (void); /* { dg-warning "'implicit_int_3' \\\[-Wimplicit-int\\]" } */
implicit_int_4 (i) /* { dg-warning "return type defaults to 'int' \\\[-Wimplicit-int\\\]" } */
/* { dg-warning "type of 'i' defaults to 'int' \\\[-Wimplicit-int\\\]" "" { target *-*-*} .-1 } */
{
  (const) 0; /* { dg-warning "type defaults to 'int' in type name \\\[-Wimplicit-int\\\]" } */
}

extern int missing_parameter_type (i); /* { dg-warning "parameter names \\\(without types\\\) in function declaration \\\[-Wdeclaration-missing-parameter-type\\\]" } */


int *
int_conversion_1 (int flag)
{
  void f2 (int *);
  flag ? "1" : 1; /* { dg-warning "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" } */
  flag ? 1 : "1"; /* { dg-warning "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" } */
  f2 (flag); /* { dg-warning "passing argument 1 of 'f2' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" } */
  {
    int i1 = &flag; /* { dg-warning "initialization of 'int' from 'int \\\*' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" } */
    i1 = &flag; /* { dg-warning "assignment to 'int' from 'int \\\*' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" } */
  }
  return flag; /* { dg-warning "returning 'int' from a function with return type 'int \\\*' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" } */
}

int
int_conversion_2 (int flag)
{
  void f3 (int);
  f3 (&flag); /* { dg-warning "passing argument 1 of 'f3' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" } */
  {
    int *i1 = flag; /* { dg-warning "initialization of 'int \\\*' from 'int' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" } */
    i1 = flag; /* { dg-warning "assignment to 'int \\\*' from 'int' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" } */
  }
  return &flag; /* { dg-warning "returning 'int \\\*' from a function with return type 'int' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" } */
}

int *
incompatible_pointer_types (int flag)
{
  void f4 (int *);
  flag ? __builtin_abs : __builtin_labs; /* { dg-warning "pointer type mismatch in conditional expression \\\[-Wincompatible-pointer-types\\\]" } */
  {
    int *p1 = __builtin_abs; /* { dg-warning "initialization of 'int \\\*' from pointer to '__builtin_abs' with incompatible type 'int \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" } */
    p1 = __builtin_abs; /* { dg-warning "assignment to 'int \\\*' from pointer to '__builtin_abs' with incompatible type 'int \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" } */
  }
  {
    int *p2 = incompatible_pointer_types; /* { dg-warning "initialization of 'int \\\*' from incompatible pointer type 'int \\\* \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" } */
    p2 = incompatible_pointer_types; /* { dg-warning "assignment to 'int \\\*' from incompatible pointer type 'int \\\* \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" } */
    {
      int *p3 = &p2; /* { dg-warning "initialization of 'int \\\*' from incompatible pointer type 'int \\\*\\\*' \\\[-Wincompatible-pointer-types\\\]" } */
      p3 = &p2; /* { dg-warning "assignment to 'int \\\*' from incompatible pointer type 'int \\\*\\\*' \\\[-Wincompatible-pointer-types\\\]" } */
    }
    f4 (&p2); /* { dg-warning "passing argument 1 of 'f4' from incompatible pointer type \\\[-Wincompatible-pointer-types\\\]" } */
  }
  if (flag)
    return __builtin_abs; /* { dg-warning "returning pointer to '__builtin_abs' of type 'int \\\(\\\*\\\)\\\(int\\\)' from a function with incompatible type 'int \\\*' \\\[-Wincompatible-pointer-types\\\]" } */
  else
    return incompatible_pointer_types; /* { dg-warning "returning 'int \\\* \\\(\\\*\\\)\\\(int\\\)' from a function with incompatible return type 'int \\\*' \\\[-Wincompatible-pointer-types\\\]" } */
}

void
return_mismatch_1 (void)
{
  return 0; /* { dg-warning "'return' with a value, in function returning void \\\[-Wreturn-mismatch\\\]" } */
}

int
return_mismatch_2 (void)
{
  return; /* { dg-warning "return' with no value, in function returning non-void \\\[-Wreturn-mismatch\\\]" } */
}
