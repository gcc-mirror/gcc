/* Test -segs_read_write_addr.  */
/* Contributed by Devang Patel  <dpatel@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-segs_read_only_addr 4000 -segs_read_write_addr 8000 -dynamiclib" } */


int
main ()
{
  return 0;
}

