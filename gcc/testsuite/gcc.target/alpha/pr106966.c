/* PR target/106906 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbuild-constants" } */

void
do_console (unsigned short *vga)
{
  vga[0] = 'H';
  vga[1] = 'e';
  vga[2] = 'l';
  vga[3] = 'l';
  vga[4] = 'o';
}
