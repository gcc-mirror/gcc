/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

/* A bug in the builtin definition made__builtin_tbeginc to have an
   integer return argument.  */
void
must_not_compile1 (void)
{
  int rc = __builtin_tbeginc (); /* { dg-error "void value not ignored as it ought to be" } */
}
