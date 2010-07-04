/* Test the legacy option -Werror-implicit-function-declaration */
/* { dg-do compile } */
/* { dg-options "-std=c89 -Werror-implicit-function-declaration" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
void f(void) 
{ 
  puts("Hello"); /* { dg-error "implicit declaration of function" } */
}
