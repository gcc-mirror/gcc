/* { dg-options "--coverage -fpath-coverage" } */
/* { dg-do run { target native } } */

/* BEGIN paths
   summary: 1/2
   expect covered: 13(true) 14 17
*/
void
covered (int a)
/* END */
{
  int v = 0;
  if (a)
    v++;
  else
    v--;
}

/* BEGIN paths
   summary: 1/1
   expect covered: 24 */
int main ()
{
  covered (1);
}

/* { dg-final { run-gcov prime-paths { --prime-paths-lines=covered gcov-33.c } } } */
