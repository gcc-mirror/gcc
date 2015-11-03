/* PR c/66415 */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

void
fn1 (void)
{
  __builtin_printf                                ("xxxxxxxxxxxxxxxxx%dxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"); /* { dg-warning "71:format" } */
}
