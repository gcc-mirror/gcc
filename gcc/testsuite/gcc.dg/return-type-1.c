/* Missing warning for falling off the end of a non-void function.
   egcs-1.1.2 passes, egcs-ss-19990428 fails.
   http://gcc.gnu.org/ml/gcc-bugs/1999-03n/msg00221.html */
/* { dg-do compile } */
/* { dg-options "-O -Wreturn-type" } */
int
foo(void)
{
} /* { dg-warning "control reaches end of non-void function" "warning for falling off end of non-void function" } */
