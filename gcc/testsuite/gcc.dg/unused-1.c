/* Missing `unused parameter' warning.
   egcs-1.1.2 fails, egcs-ss-19990418 passes.
   http://www.cygnus.com/ml/egcs-bugs/1998-Sep/0199.html */
/* { dg-do compile } */
/* { dg-options "-O -Wall -W" } */
int
f(c)
     char c; /* { dg-warning "unused parameter" "unused parameter warning" } */
{
  return 0;
}
