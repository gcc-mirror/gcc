/* PR 13129 */
/* { dg-options "-Wshadow" } */

extern struct foo bar;
void dummy()
{
  extern struct foo bar;
}
