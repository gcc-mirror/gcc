/* PR middle-end/38360 */
/* { dg-require-effective-target untyped_assembly } */

int
main ()
{
  fputs ("");
  fputs_unlocked ("");
  return 0;
}
