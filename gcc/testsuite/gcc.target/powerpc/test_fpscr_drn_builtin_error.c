/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target hard_dfp } */
/* { dg-options "-O2 -std=c99" } */

int main ()
{

  /* Test builin with out of range arguments. The builtin
     __builtin_set_fpscr_drn() also support a variable as an argument but
     can't test variable value at compile time.  */

  __builtin_set_fpscr_drn(-1);  /* { dg-error "argument 1 must be a variable or a literal between 0 and 7, inclusive" } */ 
  __builtin_set_fpscr_drn(8);   /* { dg-error "argument 1 must be a variable or a literal between 0 and 7, inclusive" } */ 

}

