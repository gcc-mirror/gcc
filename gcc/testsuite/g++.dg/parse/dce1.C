/* Make sure that dead code isn't eliminated too early, avoiding 
   detection of errors.  */
/* { dg-do compile } */

void foo(void)
{
  if (0)
    break;		/* { dg-error "" } */
  if (1)
    ;
  else
    continue;		/* { dg-error "" } */
}
