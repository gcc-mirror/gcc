/* Verify proper errors are generated for functions taking too many
   arguments.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo (int a1,  /* { dg-error "too many function arguments" } */
     int a2,
     int a3,
     int a4,
     int a5,
     int a6)
{
  return a6;
}
