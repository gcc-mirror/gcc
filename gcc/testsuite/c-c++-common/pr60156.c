/* PR c/60156 */
/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */

int
main (int argc, char *argv[], ...) /* { dg-warning "declared as variadic function" } */
{
  return 0;
}
