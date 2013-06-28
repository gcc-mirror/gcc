/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

/* Test-case contains no array notation but is compiled with -fcilkplus.  
   It will still print the too few arguments func, thereby saying the
   if-statement after the for-loop to check for !flag_enable_cilkplus ||
   !is_cilkplus_reduce_function (fundecl) is not valid is always taken.  */

int func (int, int); /* { dg-message "declared here" } */

int main (void)
{
  int a = 5, b = 2;
  return func (a); /* { dg-error "too few arguments to function" } */
}
