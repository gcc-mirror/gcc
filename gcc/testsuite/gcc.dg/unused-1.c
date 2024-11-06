/* Missing `unused parameter' warning.
   egcs-1.1.2 fails, egcs-ss-19990418 passes.
   http://gcc.gnu.org/ml/gcc-bugs/1998-09/msg00199.html */
/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition -O -Wall -W" } */
int
f(c)
     char c; /* { dg-warning "unused parameter" "unused parameter warning" } */
{
  return 0;
}
