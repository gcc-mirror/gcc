/* Test case to check if the compiler generates an error message
   when the default version of a multiversioned function is absent
   and its pointer is taken.  */

/* { dg-do compile } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O2" } */

int __attribute__ ((target ("sse")))
foo ()
{
  return 1;
}
int __attribute__ ((target ("popcnt")))
foo ()
{
  return 0;
}

int main ()
{
  int (*p)() = &foo; /* { dg-error "use of multiversioned function without a default" {} } */
  return (*p)();
}
