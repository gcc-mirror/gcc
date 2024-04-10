/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int b;
void a() __attribute__((__noreturn__));
void c() {
  char *buf;
  int bufsz = 64;
  while (b) {
    !bufsz ? a(), 0 : *buf++ = bufsz--;
    b -= 4;
  }
}
