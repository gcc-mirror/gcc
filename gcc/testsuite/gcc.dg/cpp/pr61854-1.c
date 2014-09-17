/* PR c/61854 */
/* { dg-do run } */
/* { dg-options "-std=c89" } */

int
main (void)
{
  int i = 1 //**/ 2
  ;
  int j = 1 //**/ 2
  ;
  if (i != 0 || j != 0)
    __builtin_abort ();  
  return 0;
}
