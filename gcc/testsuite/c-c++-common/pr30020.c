/* PR c/30020 */
/* { dg-do compile } */

int
foo (unsigned char c)
{
  switch (c) { case 42: case -1: return -1; }; /* { dg-warning "25:case label value" } */
  switch (c) { case 42: case 300: return -1; }; /* { dg-warning "25:case label value" } */
  switch (c) { case 42: case -1 ... 2: return -1; }; /* { dg-warning "25:lower value in case" } */
  switch (c) { case 42: case 250 ... 300: return -1; }; /* { dg-warning "25:upper value in case" } */
  return 0;
}
