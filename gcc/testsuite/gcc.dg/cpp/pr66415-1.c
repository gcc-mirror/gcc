/* PR c/66415 */
/* { dg-do compile } */
/* { dg-options "-Wformat -fdiagnostics-show-caret" } */

void
fn1 (void)
{
  __builtin_printf                                ("xxxxxxxxxxxxxxxxx%dxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"); /* { dg-warning "71:format" } */

/* { dg-begin-multiline-output "" }
   __builtin_printf                                ("xxxxxxxxxxxxxxxxx%dxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                                                                      ~^
   { dg-end-multiline-output "" } */

}
