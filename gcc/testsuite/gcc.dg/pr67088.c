/* PR c/67088 */
/* { dg-do compile } */
/* { dg-options "-Wpedantic -std=c90" } */

enum E { A = 2 };
int j;
float f;
struct S1 {
  double b1:1; /* { dg-error "10:has invalid type" } */
  int b2:j; /* { dg-error "7:width not an integer constant" } */
  int b3:f; /* { dg-error "7:width not an integer constant" } */
  int b4:(int) __builtin_log (100); /* { dg-warning "7:width not an integer constant" } */
  int b5:-2; /* { dg-error "7:negative width" } */
  int b6:0; /* { dg-error "7:zero width" } */
  long int b7:32; /* { dg-warning "12:type of bit-field" } */
  int b8:sizeof (int) * __CHAR_BIT__ * 2; /* { dg-error "7:width of" } */
  __extension__ enum E b9:1; /* { dg-warning "24:is narrower" } */
};
