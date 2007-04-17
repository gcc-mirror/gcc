/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow" } */

/* From PR 31522.  */

int f (int x) {
  int y;
  if (x <= 4) y = 1;
  else y = x / 4;
  return y <= 0;
}
