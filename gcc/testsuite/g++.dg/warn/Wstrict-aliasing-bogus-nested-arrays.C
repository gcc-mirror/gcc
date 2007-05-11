/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */

int foo () {
  int buffer[10][10];
  int* pi = &buffer[0][0];  /* { dg-bogus "same element type" } */
  *pi = 10;
  return buffer[0][0];
}
