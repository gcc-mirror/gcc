/* Missing warning for falling off the end of a non-void function.
   egcs-1.1.2 passes, egcs-ss-19990428 fails.
   http://egcs.cygnus.com/ml/egcs-bugs/1999-03/msg00220.html */
/* { dg-do compile } */
/* { dg-options "-O -Wreturn-type" } */
int
foo(void)
{
} /* { dg-warning "control reaches end of non-void function" "warning for falling off end of non-void function" } */
