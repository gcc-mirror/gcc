/* PR middle-end/106080 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wdangling-pointer" } */

void
foo (void **failaddr)
{
  *failaddr = ({ __label__ __here; __here: &&__here; }); /* { dg-bogus "address of local variable" } */
}
