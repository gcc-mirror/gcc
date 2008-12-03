/* PR middle-end/38360 */

int
main ()
{
  fputs ("");
  fputs_unlocked ("");
  return 0;
}
