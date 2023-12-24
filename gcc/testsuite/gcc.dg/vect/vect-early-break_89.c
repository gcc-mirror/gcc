/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-w" } */

char *a;
extern void d();
void b() {
  int c = 0;
  while (c < 16) {
    switch (a[c]) {
    case '"':
    case '\'':
      c++;
      continue;
    }
    break;
  }
  if (c)
    d();
}
