/* Test -segaddr.  */
/* Contributed by Devang Patel  <dpatel@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-segaddr __DATA 4000" } */


int
main ()
{
  return 0;
}

