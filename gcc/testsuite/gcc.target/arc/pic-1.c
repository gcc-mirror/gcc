/* Tests how complex pic constant expressions are handled.  */
/* { dg-do assemble } */
/* { dg-skip-if "PIC not available for ARC6xx" { arc6xx } } */
/* { dg-options "-mno-sdata -w -Os -fpic" } */

a() {
  char *b = "";
  char c;
  int d = &c - " \n\t\v\b\r\f\a/\0";
  e(b[d]);
}
