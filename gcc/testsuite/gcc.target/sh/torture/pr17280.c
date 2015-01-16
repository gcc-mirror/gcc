/* Check that there are no problems with .uses labels when branch relaxation
   is enabled.  */
/* { dg-do assemble }  */
/* { dg-additional-options "-mrelax" }  */

extern void foo (int);

int
main (void)
{
  foo (7);
  return 0;
}
