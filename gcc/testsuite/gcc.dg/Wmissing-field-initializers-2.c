/* PR c/84685 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct T {
  int a;
  int *b;
  int c;
}; 

struct T t = { .b = (int[]){1} }; /* { dg-bogus "missing initializer" } */
