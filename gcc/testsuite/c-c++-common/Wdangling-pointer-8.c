/* Verify -Wdangling-pointer is issued only once.
   { dg-do compile }
   { dg-options "-O -Wall" } */

void *p;

void escape_global_warn_once (void)
{
  int x[5];

  p = &x[3];        // { dg-regexp "\[^\n\r\]+: warning: \[^\n\r\]+ \\\[-Wdangling-pointer.?\\\]" "message" }
}


void escape_param_warn_once (void **p)
{
  int x[5];

  *p = &x[3];       // { dg-regexp "\[^\n\r\]+: warning: \[^\n\r\]+ \\\[-Wdangling-pointer.?\\\]" "message" }
}
