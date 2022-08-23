/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2 -std=c99" } */

int main ()
{

  /* Test builin with out of range arguments. Can only test for constant
     int arguments.  The builtins __builtin_set_fpscr_rn() also supports a
     variable as an argument but can't test variable value at compile time.  */

  __builtin_mtfsb0(-1);  /* { dg-error "argument 1 must be a literal between 0 and 31, inclusive" } */
  __builtin_mtfsb0(32);  /* { dg-error "argument 1 must be a literal between 0 and 31, inclusive" } */

  __builtin_mtfsb1(-1);  /* { dg-error "argument 1 must be a literal between 0 and 31, inclusive" } */
  __builtin_mtfsb1(32);  /* { dg-error "argument 1 must be a literal between 0 and 31, inclusive" } */ 

  __builtin_set_fpscr_rn(-1);  /* { dg-error "argument 1 must be a variable or a literal between 0 and 3, inclusive" } */ 
  __builtin_set_fpscr_rn(4);   /* { dg-error "argument 1 must be a variable or a literal between 0 and 3, inclusive" } */ 
}

