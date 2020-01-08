/* PR c/66415 */
/* { dg-do compile } */
/* { dg-options "-Wformat -fdiagnostics-show-caret" } */
/* { dg-set-compiler-env-var COLUMNS "83" } */

void
fn1 (void)
{
  __builtin_printf                                ("xxxxxxxxxxxxxxxxx%dxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"); /* { dg-warning "71:format" } */

/* { dg-begin-multiline-output "" }
   __builtin_printf                                ("xxxxxxxxxxxxxxxxx%dxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                                                                      ~^
                                                                       |
                                                                       int
   { dg-end-multiline-output "" } */

}
